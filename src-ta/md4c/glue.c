/*
 * glue.c — ta C module "md4c": markdown -> flat SAX event list.
 *
 *   md4c.events(path) -> List(Event)   (nil if the file cannot be read)
 *
 * Architecture (go.blog .pge/spec.md): this C layer only translates md4c's
 * SAX callbacks into a flat, document-ordered event list. It builds no
 * tree and no ADT — the ta side (src-ta/lib/md.ta) folds the events into
 * the html Node ADT and applies the cora-wrap.c special cases (task-list
 * items, checkbox attrs, ol start attr, code-block language quirk).
 *
 * Event shape: every event is a pair  (sym . payload).  The full grammar
 * is documented at the top of src-ta/lib/md.ta; the two sides must stay
 * in sync.
 *
 * md4c configuration is byte-identical to cora/lib/md4c/wrap.c:
 * parser flags MD_FLAG_STRIKETHROUGH | MD_FLAG_TASKLISTS; MD_TEXT_HTML and
 * MD_TEXT_SOFTBR are dropped; MD_BLOCK_DOC/HTML emit nothing; MD_BLOCK_TH/
 * TD emit nothing (cora's enter switch has no th/td case either, and no
 * blog post contains a markdown table).
 *
 * GC discipline: the event accumulator `acc` and each in-flight payload
 * are rooted with gc_root_push while further allocations may run
 * (see ta.h GC_ROOTS_SCOPE docs, rule 2).
 */

#include "ta.h"
#include "md4c.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- pre-interned event symbols (set in vm_load_self) ---- */

static Val sym_e_p, sym_e_quote, sym_e_ul, sym_e_ol, sym_e_li, sym_e_h,
    sym_e_code, sym_e_hr, sym_e_table, sym_e_thead, sym_e_tbody, sym_e_tr;
static Val sym_l_p, sym_l_quote, sym_l_ul, sym_l_ol, sym_l_li, sym_l_h,
    sym_l_code, sym_l_table, sym_l_thead, sym_l_tbody, sym_l_tr;
static Val sym_s_em, sym_s_strong, sym_s_code, sym_s_del, sym_s_a, sym_s_img,
    sym_sl_em, sym_sl_strong, sym_sl_code, sym_sl_del, sym_sl_a, sym_sl_img;
static Val sym_text, sym_br;

/* ---- SAX state ---- */

typedef struct {
    Proc *p;
    int acc_slot; /* index into p->gc_roots of the parked accumulator */
} md4c_state;

/* Append one (sym . payload) event.
 *
 * GC semantics: gc_root_push stores a COPY of the Val; the copy is fixed up
 * in place when the (copying) GC runs; gc_root_pop returns that refreshed
 * copy. C locals / C-struct fields are NOT refreshed by GC — after any
 * allocation they may point into the moved heap, so the accumulator is
 * never kept in a C variable: it lives in the parked root slot
 * s->acc_slot for the whole parse, and every read of it goes through that
 * slot (which GC / proc_grow keep correct).
 *
 * val_pair roots car and cdr itself before its allocation, so a freshly
 * built `payload` and the slot-refreshed `ev` are safe to pass directly. */
static void md4c_event(md4c_state *s, Val sym, Val payload) {
    Proc *p = s->p;
    Val ev = val_pair(p, sym, payload); /* ev fresh; alloc rooted its args */
    Val acc = p->gc_roots[s->acc_slot]; /* slot kept current by GC fixup */
    Val head = val_pair(p, ev, acc);
    p->gc_roots[s->acc_slot] = head;    /* re-park the new head */
}

static void md4c_event_nil(md4c_state *s, Val sym) { md4c_event(s, sym, val_nil()); }

/* Concatenate all substrings of an MD_ATTRIBUTE (href/src/title/lang)
 * into one heap string. Escaping is per-character and independent, so
 * pre-concatenating chunks is byte-identical to cora's per-chunk
 * append + sxml->xml escaping. */
static Val md4c_attr_text(md4c_state *s, const MD_ATTRIBUTE *attr) {
    if (attr->text == NULL)
        return val_nil();
    MD_SIZE total = attr->size;
    char *buf = malloc(total ? total : 1);
    if (!buf)
        return val_nil();
    MD_SIZE pos = 0;
    for (int i = 0; attr->substr_offsets[i] < attr->size; i++) {
        MD_SIZE off = attr->substr_offsets[i];
        MD_SIZE len = attr->substr_offsets[i + 1] - off;
        memcpy(buf + pos, attr->text + off, len);
        pos += len;
    }
    Val v = val_string(s->p, buf, (int)total);
    free(buf);
    return v;
}

/* ---- block callbacks ---- */

static int enter_block(MD_BLOCKTYPE type, void *detail, void *userdata) {
    md4c_state *s = (md4c_state *)userdata;
    switch (type) {
    case MD_BLOCK_DOC:
    case MD_BLOCK_HTML:
    case MD_BLOCK_TH:
    case MD_BLOCK_TD:
        /* cora wrap.c: DOC/HTML are noops; TH/TD have no enter case
         * (default -> assert, a no-op in release builds). */
        break;
    case MD_BLOCK_QUOTE:
        md4c_event_nil(s, sym_e_quote);
        break;
    case MD_BLOCK_UL:
        md4c_event_nil(s, sym_e_ul);
        break;
    case MD_BLOCK_OL: {
        const MD_BLOCK_OL_DETAIL *det = (const MD_BLOCK_OL_DETAIL *)detail;
        /* cora: no start attribute when start == 1. */
        unsigned start = det->start;
        md4c_event(s, sym_e_ol, val_int(start ? start : 1));
        break;
    }
    case MD_BLOCK_LI: {
        const MD_BLOCK_LI_DETAIL *det = (const MD_BLOCK_LI_DETAIL *)detail;
        /* payload 0 = plain <li>; otherwise the task mark char
         * (cora checks 'x'/'X' for the "checked" attribute). */
        int mark = det->is_task ? (int)det->task_mark : 0;
        md4c_event(s, sym_e_li, val_int(mark));
        break;
    }
    case MD_BLOCK_HR:
        md4c_event_nil(s, sym_e_hr);
        break;
    case MD_BLOCK_H: {
        const MD_BLOCK_H_DETAIL *det = (const MD_BLOCK_H_DETAIL *)detail;
        md4c_event(s, sym_e_h, val_int(det->level));
        break;
    }
    case MD_BLOCK_CODE: {
        const MD_BLOCK_CODE_DETAIL *det = (const MD_BLOCK_CODE_DETAIL *)detail;
        /* lang string, or nil (cora: class attr only when lang.text !=
         * NULL). */
        md4c_event(s, sym_e_code, md4c_attr_text(s, &det->lang));
        break;
    }
    case MD_BLOCK_P:
        md4c_event_nil(s, sym_e_p);
        break;
    case MD_BLOCK_TABLE:
        md4c_event_nil(s, sym_e_table);
        break;
    case MD_BLOCK_THEAD:
        md4c_event_nil(s, sym_e_thead);
        break;
    case MD_BLOCK_TBODY:
        md4c_event_nil(s, sym_e_tbody);
        break;
    case MD_BLOCK_TR:
        md4c_event_nil(s, sym_e_tr);
        break;
    default:
        break;
    }
    return 0;
}

static int leave_block(MD_BLOCKTYPE type, void *detail, void *userdata) {
    md4c_state *s = (md4c_state *)userdata;
    (void)detail;
    switch (type) {
    case MD_BLOCK_DOC:
    case MD_BLOCK_HR:
    case MD_BLOCK_HTML:
    case MD_BLOCK_TH:
    case MD_BLOCK_TD:
        break;
    case MD_BLOCK_QUOTE:
        md4c_event_nil(s, sym_l_quote);
        break;
    case MD_BLOCK_UL:
        md4c_event_nil(s, sym_l_ul);
        break;
    case MD_BLOCK_OL:
        md4c_event_nil(s, sym_l_ol);
        break;
    case MD_BLOCK_LI:
        md4c_event_nil(s, sym_l_li);
        break;
    case MD_BLOCK_H:
        md4c_event_nil(s, sym_l_h);
        break;
    case MD_BLOCK_CODE:
        /* one leave event; the ta fold closes both <code> and <pre>. */
        md4c_event_nil(s, sym_l_code);
        break;
    case MD_BLOCK_P:
        md4c_event_nil(s, sym_l_p);
        break;
    case MD_BLOCK_TABLE:
        md4c_event_nil(s, sym_l_table);
        break;
    case MD_BLOCK_THEAD:
        md4c_event_nil(s, sym_l_thead);
        break;
    case MD_BLOCK_TBODY:
        md4c_event_nil(s, sym_l_tbody);
        break;
    case MD_BLOCK_TR:
        md4c_event_nil(s, sym_l_tr);
        break;
    default:
        break;
    }
    return 0;
}

/* ---- span callbacks ---- */

static int enter_span(MD_SPANTYPE type, void *detail, void *userdata) {
    md4c_state *s = (md4c_state *)userdata;
    switch (type) {
    case MD_SPAN_EM:
        md4c_event_nil(s, sym_s_em);
        break;
    case MD_SPAN_STRONG:
        md4c_event_nil(s, sym_s_strong);
        break;
    case MD_SPAN_CODE:
        md4c_event_nil(s, sym_s_code);
        break;
    case MD_SPAN_DEL:
        md4c_event_nil(s, sym_s_del);
        break;
    case MD_SPAN_A: {
        const MD_SPAN_A_DETAIL *det = (const MD_SPAN_A_DETAIL *)detail;
        /* payload: (href . title); title is nil when absent. Park href in
         * a root slot: building title allocates, which can move the heap
         * and invalidate the href C local. */
        Val href = md4c_attr_text(s, &det->href);
        gc_root_push(s->p, href);
        Val title = val_nil();
        if (det->title.text != NULL)
            title = md4c_attr_text(s, &det->title);
        href = gc_root_pop(s->p); /* refreshed after title's alloc */
        Val payload = val_pair(s->p, href, title);
        md4c_event(s, sym_s_a, payload);
        break;
    }
    case MD_SPAN_IMG: {
        const MD_SPAN_IMG_DETAIL *det = (const MD_SPAN_IMG_DETAIL *)detail;
        /* payload: src string (cora renders only src; a title attr is
         * dropped, alt text arrives via text callbacks). */
        md4c_event(s, sym_s_img, md4c_attr_text(s, &det->src));
        break;
    }
    default:
        /* U / LATEXMATH / WIKILINK: parser flags never enable them. */
        break;
    }
    return 0;
}

static int leave_span(MD_SPANTYPE type, void *detail, void *userdata) {
    md4c_state *s = (md4c_state *)userdata;
    (void)detail;
    switch (type) {
    case MD_SPAN_EM:
        md4c_event_nil(s, sym_sl_em);
        break;
    case MD_SPAN_STRONG:
        md4c_event_nil(s, sym_sl_strong);
        break;
    case MD_SPAN_CODE:
        md4c_event_nil(s, sym_sl_code);
        break;
    case MD_SPAN_DEL:
        md4c_event_nil(s, sym_sl_del);
        break;
    case MD_SPAN_A:
        md4c_event_nil(s, sym_sl_a);
        break;
    case MD_SPAN_IMG:
        md4c_event_nil(s, sym_sl_img);
        break;
    default:
        break;
    }
    return 0;
}

/* ---- text callback ---- */

static int text_cb(MD_TEXTTYPE type, const MD_CHAR *text, MD_SIZE size,
    void *userdata) {
    md4c_state *s = (md4c_state *)userdata;
    switch (type) {
    case MD_TEXT_BR:
        md4c_event_nil(s, sym_br);
        break;
        case MD_TEXT_HTML:
        /* cora wrap.c: dropped (it conflicts with sxml). */
        break;
    default:
        /* NORMAL, ENTITY, CODE, ESCAPED, SOFTBR, ... : raw text passed
         * through as-is — cora's text_callback has no SOFTBR case either,
         * so a soft break reaches this default and md4c delivers it as
         * the 1-byte text "\n" (the newlines visible inside cora's <p>
         * output). The ta side escapes it when rendering (matches cora's
         * string sxml nodes). */
        md4c_event(s, sym_text, val_string(s->p, (const char *)text, (int)size));
        break;
    }
    return 0;
}

static void debug_log(const char *msg, void *userdata) {
    (void)msg;
    (void)userdata;
}

/* ---- module entry ---- */

static Val md4c_events(VM *vm, Val *args, int nargs) {
    (void)vm;
    (void)nargs;
    Proc *p = tls_current_proc;
    if (!p || !val_is_string(args[0]))
        return val_nil();
    HeapString *hs = val_get_string(args[0]);

    FILE *f = fopen(hs->data, "rb");
    if (!f)
        return val_nil();
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        return val_nil();
    }
    long sz = ftell(f);
    if (sz < 0) {
        fclose(f);
        return val_nil();
    }
    rewind(f);
    char *buf = malloc((size_t)sz + 1);
    if (!buf) {
        fclose(f);
        return val_nil();
    }
    size_t got = fread(buf, 1, (size_t)sz, f);
    fclose(f);

    md4c_state s;
    s.p = p;
    s.acc_slot = -1;

    static const unsigned parser_flags = MD_FLAG_STRIKETHROUGH |
        MD_FLAG_TASKLISTS; /* cora wrap.c flags */

    MD_PARSER parser = {0,
        parser_flags,
        enter_block,
        leave_block,
        enter_span,
        leave_span,
        text_cb,
        debug_log,
        NULL};

    /* Park the accumulator in a root slot for the whole parse. The slot
     * (not a C variable) is the single source of truth for the current
     * head: GC and proc_grow both fix up gc_roots in place, so it stays
     * valid across every allocation the callbacks make. */
    int saved_count = p->gc_root_count;
    gc_root_push(p, val_nil());
    s.acc_slot = p->gc_root_count - 1;
    int ret = md_parse(buf, (MD_SIZE)got, &parser, &s);
    Val acc = p->gc_roots[s.acc_slot];
    p->gc_root_count = saved_count;
    free(buf);
    if (ret != 0)
        return val_nil();

    /* Reverse the accumulator back to document order (event level only).
     * Per iteration: park event, cell and in-flight result, allocate the
     * new head, then pop all three (explicit, in push order) — never use
     * a C-local Val across an allocation. */
    /* The iterator itself must be parked: val_pair allocates and can run
     * GC/proc_grow, which moves every heap object — a C-local `it` would
     * go stale. Park it (and the in-flight result) each iteration. */
    int saved2 = p->gc_root_count;
    gc_root_push(p, acc);
    gc_root_push(p, val_nil()); /* slot for the in-flight result */
    int it_slot = p->gc_root_count - 2;
    int res_slot = p->gc_root_count - 1;
    while (!val_is_nil(p->gc_roots[it_slot])) {
        Val cell = p->gc_roots[it_slot];
        Val e = val_get_car(cell);
        Val next = val_get_cdr(cell);
        p->gc_roots[it_slot] = next;
        p->gc_roots[res_slot] = val_pair(p, e, p->gc_roots[res_slot]);
    }
    Val rev = p->gc_roots[res_slot];
    p->gc_root_count = saved2;
    return rev;
}

TaFunc md4c_funcs[] = {{"events", md4c_events, 1}, {NULL, NULL, 0}};

void vm_load_self(VM *vm) {
    /* Pre-intern all event symbols at load time (see docs/c-module.md:
     * avoid concurrent runtime interning). */
    sym_e_p = val_symbol(vm_intern_symbol(vm, "e-p"));
    sym_e_quote = val_symbol(vm_intern_symbol(vm, "e-quote"));
    sym_e_ul = val_symbol(vm_intern_symbol(vm, "e-ul"));
    sym_e_ol = val_symbol(vm_intern_symbol(vm, "e-ol"));
    sym_e_li = val_symbol(vm_intern_symbol(vm, "e-li"));
    sym_e_h = val_symbol(vm_intern_symbol(vm, "e-h"));
    sym_e_code = val_symbol(vm_intern_symbol(vm, "e-code"));
    sym_e_hr = val_symbol(vm_intern_symbol(vm, "e-hr"));
    sym_e_table = val_symbol(vm_intern_symbol(vm, "e-table"));
    sym_e_thead = val_symbol(vm_intern_symbol(vm, "e-thead"));
    sym_e_tbody = val_symbol(vm_intern_symbol(vm, "e-tbody"));
    sym_e_tr = val_symbol(vm_intern_symbol(vm, "e-tr"));
    sym_l_p = val_symbol(vm_intern_symbol(vm, "l-p"));
    sym_l_quote = val_symbol(vm_intern_symbol(vm, "l-quote"));
    sym_l_ul = val_symbol(vm_intern_symbol(vm, "l-ul"));
    sym_l_ol = val_symbol(vm_intern_symbol(vm, "l-ol"));
    sym_l_li = val_symbol(vm_intern_symbol(vm, "l-li"));
    sym_l_h = val_symbol(vm_intern_symbol(vm, "l-h"));
    sym_l_code = val_symbol(vm_intern_symbol(vm, "l-code"));
    sym_l_table = val_symbol(vm_intern_symbol(vm, "l-table"));
    sym_l_thead = val_symbol(vm_intern_symbol(vm, "l-thead"));
    sym_l_tbody = val_symbol(vm_intern_symbol(vm, "l-tbody"));
    sym_l_tr = val_symbol(vm_intern_symbol(vm, "l-tr"));
    sym_s_em = val_symbol(vm_intern_symbol(vm, "s-em"));
    sym_s_strong = val_symbol(vm_intern_symbol(vm, "s-strong"));
    sym_s_code = val_symbol(vm_intern_symbol(vm, "s-code"));
    sym_s_del = val_symbol(vm_intern_symbol(vm, "s-del"));
    sym_s_a = val_symbol(vm_intern_symbol(vm, "s-a"));
    sym_s_img = val_symbol(vm_intern_symbol(vm, "s-img"));
    sym_sl_em = val_symbol(vm_intern_symbol(vm, "sl-em"));
    sym_sl_strong = val_symbol(vm_intern_symbol(vm, "sl-strong"));
    sym_sl_code = val_symbol(vm_intern_symbol(vm, "sl-code"));
    sym_sl_del = val_symbol(vm_intern_symbol(vm, "sl-del"));
    sym_sl_a = val_symbol(vm_intern_symbol(vm, "sl-a"));
    sym_sl_img = val_symbol(vm_intern_symbol(vm, "sl-img"));
    sym_text = val_symbol(vm_intern_symbol(vm, "text"));
    sym_br = val_symbol(vm_intern_symbol(vm, "br"));

    vm_register_module(vm, "md4c", md4c_funcs, 1);
}