#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>

using namespace std;

// Structure to represent a node in the binary tree
struct Node {
    int data;
    Node* left;
    Node* right;

    Node(int val) : data(val), left(nullptr), right(nullptr) {}
};

// Function to build a single node of the binary tree
Node* buildNode(istringstream& iss) {
    char ch;
    if (!(iss >> ch)) {
        cerr << "Error reading from file\n";
        exit(1);
    }
    if (ch == '0') {
        return nullptr;
    } else {
        int num = ch - '0';
        Node* node = new Node(num);
        node->left = buildNode(iss);
        node->right = buildNode(iss);
        return node;
    }
}

// Function to read input from a file and build the binary tree
Node* buildTree(const string& filename) {
    ifstream file(filename);
    if (!file) {
        cerr << "Error opening file: " << filename << endl;
        exit(1);
    }

    string line;
    getline(file, line);
    istringstream iss(line);
    return buildNode(iss);
}

// Function to swap the left and right children of a node recursively
void swapNodes(Node* root) {
    if (root == nullptr) {
        return;
    }
    swap(root->left, root->right);
    swapNodes(root->left);
    swapNodes(root->right);
}

// Function to perform in-order traversal of the binary tree
void inOrderTraversal(Node* root) {
    if (root == nullptr) {
        return;
    }
    inOrderTraversal(root->left);
    cout << root->data << " ";
    inOrderTraversal(root->right);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }

    Node* root = buildTree(argv[1]);
    if (root == nullptr) {
        cerr << "Error: Failed to build binary tree from input file\n";
        return 1;
    }

    // Swap nodes recursively
    swapNodes(root);

    // Perform in-order traversal to print the result
    inOrderTraversal(root);
    cout << endl;

    return 0;
}

