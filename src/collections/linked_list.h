#ifndef _LINKED_LIST_H
#define _LINKED_LIST_H

#include "../defines.h"

typedef struct LinkedListItem {
    void*                  value;
    struct LinkedListItem* next;
} LinkedListItem;

typedef struct {
    LinkedListItem* head;
    LinkedListItem* tail;
    u64             len;
} LinkedList;

typedef void (*FreeFn)(void*);

LinkedList linked_list_create(void);
void linked_list_destroy(LinkedList* list, FreeFn free_fn);

LinkedListItem* linked_list_insert_idx(LinkedList* list, u64 idx, void* value);
LinkedListItem* linked_list_insert_head(LinkedList* list, void* value);
LinkedListItem* linked_list_insert_tail(LinkedList* list, void* value);

LinkedListItem* linked_list_get_idx(LinkedList* list, u64 idx);
LinkedListItem* linked_list_get_head(LinkedList* list);
LinkedListItem* linked_list_get_tail(LinkedList* list);

LinkedListItem* linked_list_remove_idx(LinkedList* list, u64 idx);
LinkedListItem* linked_list_remove_head(LinkedList* list);
LinkedListItem* linked_list_remove_tail(LinkedList* list);

#endif
