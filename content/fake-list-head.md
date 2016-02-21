以前学数据结构，老师给我们讲带头结点的单链表和不带头结点的单链表。

对普通链表(不带头结点单链表)的操作，比如做删除或者插入，需要考虑一些特殊场景：1.判断是否空链表；2.操作头结点跟其它不一样，因为可能会修改head指针

    struct ListNode* list_delete1(struct ListNode *l, int n) {
      if (l == NULL) return NULL;
      if (l->val == n) {
        return l->next;
      }
      struct ListNode *prev = l;
      struct ListNode *cur = l->next;
      while(cur) {
        if (cur->val == n) {
          prev->next = cur->next;
          return l;
        }
        prev = cur;
        cur = cur->next;
      }
      return l;
    }

带头结点的链表浪费一个结点的空间，换来的是代码的简化，因为不需要对头结点特殊考虑，所有操作都是一致的。

自己想到的一个小技巧，有些场合可以伪造一个fake的头结点出来，简化代码。

    struct ListNode* list_delete2(struct ListNode *l, int n) {
      struct ListNode fake;
      fake.next = l;
      for (struct ListNode *prev=&fake; prev->next; prev=prev->next) {
        if (prev->next->val == n) {
          prev->next = prev->next->next;
          break;
        }
      }
      return fake.next;
    }

下面的代码是不是比上面精简了？这个技巧在平时写代码或者是[刷题什么的](https://github.com/tiancaiamao/leetcode/blob/master/147.c#L22)都可以用到。
