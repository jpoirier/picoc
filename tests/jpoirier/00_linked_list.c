#include <stdio.h>
#include <stdlib.h>


static void enqueue_tail(int d);
static void enqueue_head(int d);
static int dequeue_tail(void);
static int dequeue_head(void);

struct Node {
	struct Node *next;
	struct Node *prev;
	int data;
};

struct Node *head = NULL;
struct Node *tail = NULL;

void enqueue_tail(int d) {
	struct Node *new_node = malloc(sizeof(*new_node));

	new_node->data = d;
	new_node->next = tail;
	new_node->prev = NULL;

	if (tail != NULL)
		tail->prev = new_node;

	tail = new_node;
	if (head == NULL)
		head = tail;

// 	printf("enqueue_tail new node data: %d\n", new_node->data);
// 	printf("enqueue_tail new node: %p\n", (void*)new_node);
// 	printf("enqueue_tail next node: %p\n", (void*)new_node->next);
// 	printf("enqueue_tail prev node: %p\n", (void*)new_node->prev);
}

void enqueue_head(int d) {
	struct Node *new_node = malloc(sizeof(*new_node));
// 	printf("enqueue_head head: %p\n", (void*)head);
	new_node->data = d;
	new_node->next = NULL;
	new_node->prev = head;

	if (head != NULL)
		head->next = new_node;

	head = new_node;
	if (tail == NULL)
		tail = head;

// 	printf("enqueue_head new node data: %d\n", new_node->data);
// 	printf("enqueue_head new node: %p\n", (void*)new_node);
// 	printf("enqueue_head next node: %p\n", (void*)new_node->next);
// 	printf("enqueue_head prev node: %p\n", (void*)new_node->prev);
}

int dequeue_tail(void) {
	if (tail == NULL)
		return -1;

// 	printf("dequeue_tail tail: %p\n", (void*)tail);
// 	printf("dequeue_tail tail next: %p\n", (void*)tail->next);
// 	printf("dequeue_tail tail prev: %p\n", (void*)tail->prev);

	struct Node *t = tail;
	int d = tail->data;
	if (tail->next != NULL)
		tail->next->prev = NULL;

	tail = tail->next;
	if (t == head)
		head = NULL;

	free(t);
	return d;
}

int dequeue_head(void) {
	if (head == NULL)
		return -1;

// 	printf("A. dequeue_head head: %p\n", (void*)head);
// 	printf("A. dequeue_head head next: %p\n", (void*)head->next);
// 	printf("A. dequeue_head head prev: %p\n", (void*)head->prev);

	struct Node *t = head;
	int d = head->data;

	if (head->prev != NULL) {
		head->prev->next = NULL;
	}

	head = head->prev;

// 	printf("B. dequeue_head t: %p\n", (void*)t);
// 	printf("B. dequeue_head head: %p\n", (void*)head);
// 	printf("B. dequeue_head head next: %p\n", (void*)((head == NULL) ? NULL : head->next));
// 	printf("B. dequeue_head head prev: %p\n", (void*)((head == NULL) ? NULL : head->prev));
	if (t == tail)
		tail = NULL;

	free(t);
	return d;
}

void reverse_list(void) {
	struct Node *tmp1;
	struct Node *tmp2;

	tmp1 = tail;
	tail = head;
	head = tmp1;

	while (tmp1 != NULL) {
		tmp2 = tmp1->next;
		tmp1->next = tmp1->prev;
		tmp1->prev = tmp2;
		tmp1 = tmp2;
	}
}

#define COUNT (20)
int main(int argc, char **argv) {
	int i;
	for (i = 0; i < COUNT; i++) {
		printf("enqueue tail: %d\n", i);
		enqueue_tail(i);
	}
	for (i = 0; i < COUNT; i++) {
		printf("dequeue tail: %d\n", dequeue_tail());
	}
	for (i = 0; i < COUNT; i++) {
		printf("enqueue head: %d\n", i);
		enqueue_head(i);
	}
	for (i = 0; i < COUNT; i++) {
		printf("dequeue head: %d\n", dequeue_head());
	}
	for (i = 0; i < 10; i++) {
		printf("enqueue head: %d\n", i);
		enqueue_head(i);
	}
	for (i = 10; i < COUNT; i++) {
		printf("enqueue tail: %d\n", i);
		enqueue_tail(i);
	}
	for (i = 0; i < COUNT; i++) {
		printf("dequeue tail: %d\n", dequeue_tail());
	}
	for (i = 0; i < COUNT; i++) {
		printf("enqueue tail: %d\n", i);
		enqueue_head(i);
	}
	reverse_list();
	for (i = 0; i < COUNT; i++) {
		printf("dequeue tail: %d\n", dequeue_tail());
	}
	return 0;
}
