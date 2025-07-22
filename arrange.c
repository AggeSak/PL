#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 1000000  // Using allowed underscore separator

// Structure to represent a node in the binary tree
typedef struct Node {
  int data;
  struct Node* left;
  struct Node* right;
} Node;

// Function declarations
Node* buildNode(FILE* fp);
Node* buildTree(const char* filename);
void swapNodes(Node* root);
void inOrderTraversal(Node* root);

// Function to build a single node of the binary tree
Node* buildNode(FILE* fp) {
  char ch;
  if (fscanf(fp, " %c", &ch) != 1) {
    fprintf(stderr, "Error reading from file\n");
    exit(1);
  }
  if (ch == '0') {
    return NULL;
  } else {
    Node* node = malloc(sizeof(Node));
    if (node == NULL) {
      fprintf(stderr, "Memory allocation failed\n");
      exit(1);
    }
    node->data = ch - '0';
    node->left = buildNode(fp);
    node->right = buildNode(fp);
    return node;
  }
}

// Function to read input from a file and build the binary tree
Node* buildTree(const char* filename) {
  FILE* fp = fopen(filename, "r");
  if (fp == NULL) {
    fprintf(stderr, "Error opening file: %s\n", filename);
    exit(1);
  }

  Node* root = buildNode(fp);

  fclose(fp);
  return root;
}

// Function to swap the left and right children of a node recursively
void swapNodes(Node* root) {
  if (root == NULL) {
    return;
  }
  Node* temp = root->left;
  root->left = root->right;
  root->right = temp;
  swapNodes(root->left);
  swapNodes(root->right);
}

// Function to perform in-order traversal of the binary tree
void inOrderTraversal(Node* root) {
  if (root == NULL) {
    return;
  }
  inOrderTraversal(root->left);
  printf("%d ", root->data);
  inOrderTraversal(root->right);
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
    return 1;
  }

  Node* root = buildTree(argv[1]);

  // Swap nodes recursively
  swapNodes(root);

  // Perform in-order traversal to print the result
  inOrderTraversal(root);
  printf("fff\n");

  return 0;
}
