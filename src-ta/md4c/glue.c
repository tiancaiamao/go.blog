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
  * GC discipline: none needed — a C module call is wrapped in the
 * proc_gc_enter gate (no collection can run until it returns) and the
 * actor arena never moves after first allocation (tinyactor issue #160).
 */

#include "md4c.h"
#include "ta.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- pre-interned event symbols (set in vm_load_self) ---- */

static Val sym_e_p, sym_e_quote, sym_e_ul, sym_e_ol, sym_e_li, sym_e_h, sym_e_code, sym_e_hr,
    sym_e_table, sym_e_thead, sym_e_tbody, sym_e_tr;
static Val sym_l_p, sym_l_quote, sym_l_ul, sym_l_ol, sym_l_li, sym_l_h, sym_l_code, sym_l_table,
    sym_l_thead, sym_l_tbody, sym_l_tr;
static Val sym_s_em, sym_s_strong, sym_s_code, sym_s_del, sym_s_a, sym_s_img, sym_sl_em,
    sym_sl_strong, sym_sl_code, sym_sl_del, sym_sl_a, sym_sl_img;
static Val sym_text, sym_br;

/* ---- SAX state ---- */

typedef struct {
    Proc *p;
    Val acc; /* reversed event list accumulator */
} md4c_state;

/* Append one (sym . payload) event.
 *
 * GC semantics (tinyactor issue #160 gate model): a C module call is
 * wrapped in proc_gc_enter — no collection can run while any of these
 * callbacks execute, and the actor arena never moves after first
 * allocation. So a plain C local / struct-field Val is valid across
 * allocations and no rooting is needed (gc_root_push was removed from
 * the VM API).
 *
 * val_pair roots car and cdr itself before its allocation, so a freshly
 * built `payload` is safe to pass directly. */
static void md4c_event(md4c_state *s, Val sym, Val payload) {
    Val ev = val_pair(s->p, sym, payload); /* ev fresh; alloc rooted its args */
    s->acc = val_pair(s->p, ev, s->acc);
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
        /* cora wrap.c render_open_ol_block: no start attribute when
         * start == 1; every other value passes through — including
         * start == 0 (markdown "0. item" renders as start="0"). */
        md4c_event(s, sym_e_ol, val_int((int)det->start));
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
        /* payload: (href . title); title is nil when absent. Plain C
         * locals are safe across allocations — no GC can run inside a
         * C module call (issue #160 gate model). */
        Val href = md4c_attr_text(s, &det->href);
        Val title = val_nil();
        if (det->title.text != NULL)
            title = md4c_attr_text(s, &det->title);
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

static int text_cb(MD_TEXTTYPE type, const MD_CHAR *text, MD_SIZE size, void *userdata) {
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
    s.acc = val_nil();

    static const unsigned parser_flags =
        MD_FLAG_STRIKETHROUGH | MD_FLAG_TASKLISTS; /* cora wrap.c flags */

    MD_PARSER parser = {0,          parser_flags, enter_block, leave_block, enter_span,
                        leave_span, text_cb,      debug_log,   NULL};

    /* No rooting needed: no GC can run inside a C module call (the
     * proc_gc_enter gate defers collection until after we return), and
     * the arena never moves — the accumulator is safe in a C struct. */
    int ret = md_parse(buf, (MD_SIZE)got, &parser, &s);
    free(buf);
    if (ret != 0)
        return val_nil();

    /* Reverse the accumulator back to document order (event level only). */
    Val rev = val_nil();
    Val it = s.acc;
    while (!val_is_nil(it)) {
        rev = val_pair(p, val_get_car(it), rev);
        it = val_get_cdr(it);
    }
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