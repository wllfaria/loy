#ifndef _LINKED_LIST_H
#define _LINKED_LIST_H

#include "../defines.h"
#include "../mem/allocator.h"

typedef struct LinkedListItem {
    void*                  value;
    struct LinkedListItem* next;
} LinkedListItem;

typedef struct {
    LinkedListItem* head;
    LinkedListItem* tail;
    u64             len;
    Allocator*      allocator;
} LinkedList;

LinkedList linked_list_create(Allocator* allocator);
void linked_list_destroy(LinkedList* list);

LinkedListItem* linked_list_insert_idx(LinkedList* list, u64 idx, void* value);
LinkedListItem* linked_list_insert_head(LinkedList* list, void* value);
LinkedListItem* linked_list_insert_tail(LinkedList* list, void* value);

LinkedListItem* linked_list_get_idx(LinkedList* list, u64 idx);
LinkedListItem* linked_list_get_head(LinkedList* list);
LinkedListItem* linked_list_get_tail(LinkedList* list);

LinkedListItem* linked_list_remove_idx(LinkedList* list, u64 idx);
LinkedListItem* linked_list_remove_head(LinkedList* list);
LinkedListItem* linked_list_remove_tail(LinkedList* list);

typedef struct {
    LinkedList* list;
    u64         cursor;
} LinkedListIter;

LinkedListIter linked_list_iter(LinkedList* list);
void* linked_list_peek(LinkedListIter* iter);
void* linked_list_next(LinkedListIter* iter);

#endif
