#include <assert.h>

#include "linked_list.h"

LinkedList linked_list_create(void) {
    LinkedList list = { .head = NULL };
    return list;
}

void linked_list_destroy(LinkedList* list, FreeFn free_fn) {
    assert(list != NULL);
    if(list->len == 0) return;

    LinkedListItem* curr = list->head;
    while(curr != NULL) {
        if(free_fn) free_fn(curr->value);
        LinkedListItem* tmp = curr;
        curr = curr->next;
        free(tmp);
    }

    list->head = NULL;
    list->tail = NULL;
    list->len  = 0;
}

LinkedListItem* linked_list_insert_idx(LinkedList* list, u64 idx, void* value) {
    assert(list != NULL);
    if(idx > list->len) return NULL;
    if(idx == 0) return linked_list_insert_head(list, value);
    if(idx == list->len) return linked_list_insert_tail(list, value);

    LinkedListItem* curr = list->head;
    for(u64 i = 0; i < idx - 1; i++) {
        curr = curr->next;
    }

    LinkedListItem* item = malloc(sizeof(LinkedListItem));
    item->next  = curr->next;
    item->value = value;
    curr->next  = item;
    list->len++;
    return item;
}

LinkedListItem* linked_list_insert_head(LinkedList* list, void* value) {
    assert(list != NULL);

    LinkedListItem* item = malloc(sizeof(LinkedListItem));
    item->value = value;

    if(list->len > 0) item->next = list->head;
    else {
        item->next = NULL;
        list->tail = item;
    }

    list->head = item;
    list->len++;
    return list->head;
}

LinkedListItem* linked_list_insert_tail(LinkedList* list, void* value) {
    assert(list != NULL);

    if(list->len == 0) return linked_list_insert_head(list, value);

    LinkedListItem* item = malloc(sizeof(LinkedListItem));
    item->next       = NULL;
    item->value      = value;
    list->tail->next = item;
    list->tail       = item;
    list->len++;
    return item;
}

LinkedListItem* linked_list_get_idx(LinkedList* list, u64 idx) {
    assert(list != NULL);
    if(idx >= list->len) return NULL;
    if(idx == 0) return linked_list_get_head(list);
    if(idx == list->len - 1) return linked_list_get_tail(list);

    LinkedListItem* curr = list->head;
    for(u64 i = 0; i < idx; i++) {
        curr = curr->next;
    }

    return curr;
}

LinkedListItem* linked_list_get_head(LinkedList* list) {
    assert(list != NULL);
    return list->head;
}

LinkedListItem* linked_list_get_tail(LinkedList* list) {
    assert(list != NULL);
    return list->tail;
}

LinkedListItem* linked_list_remove_idx(LinkedList* list, u64 idx) {
    assert(list != NULL);
    if(idx >= list->len) return NULL;
    if(idx == 0) return linked_list_remove_head(list);
    if(idx == list->len - 1) return linked_list_remove_tail(list);

    if(list->len == 1) {
        LinkedListItem* item = list->head;
        item->next = NULL;
        list->head = NULL;
        list->tail = NULL;
        list->len--;
        return item;
    }

    LinkedListItem* prev = list->head;
    LinkedListItem* curr = list->head->next;

    for(u64 i = 1; i < idx; i++) {
        prev = curr;
        curr = curr->next;
    }

    prev->next = curr->next;
    list->len--;
    curr->next = NULL;
    return curr;
}

LinkedListItem* linked_list_remove_head(LinkedList* list) {
    assert(list != NULL);
    if(list->len == 0) return NULL;
    LinkedListItem* item = list->head;
    list->head = list->head->next;
    item->next = NULL;
    list->len--;
    if(list->len == 0) list->tail = NULL;
    return item;
}

LinkedListItem* linked_list_remove_tail(LinkedList* list) {
    assert(list != NULL);
    if(list->len == 0) return NULL;
    if(list->len == 1) return linked_list_remove_head(list);

    LinkedListItem* curr = list->head;
    for(u64 i = 1; i < list->len - 1; i++) {
        curr = curr->next;
    }

    LinkedListItem* item = curr->next;
    item->next = NULL; // this should already be null, but just in case.
    curr->next = NULL;
    list->tail = curr;
    list->len--;

    return item;
}

LinkedListIter linked_list_iter_create(LinkedList* list) {
    LinkedListIter iter = {
        .list   = list,
        .cursor = 0,
    };
    return iter;
}

void* linked_list_iter_peek(LinkedListIter* iter) {
    if(iter->cursor >= iter->list->len) return NULL;
    return linked_list_get_idx(iter->list, iter->cursor);
}

void* linked_list_iter_next(LinkedListIter* iter) {
    if(iter->cursor >= iter->list->len) return NULL;
    u64 pos = iter->cursor;
    iter->cursor++;
    return linked_list_get_idx(iter->list, pos);
}
