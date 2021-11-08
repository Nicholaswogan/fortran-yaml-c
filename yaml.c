#include <yaml.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

const int STRING_LENGTH = 1024;

typedef struct TypeNode TypeNode;
struct TypeNode
{
  // Node
  int type;
  
  // Scalar
  char *string;
  
  // Dictionary
  TypeNode *first_keyvaluepair;
  char *key;
  TypeNode *value;
  TypeNode *next_keyvaluepair;
  
  // List
  TypeNode *first_listitem;
  TypeNode *node;
  TypeNode *next_listitem;

};

TypeNode* create_TypeNode(){
  TypeNode *node;
  node = (TypeNode*) malloc(sizeof(TypeNode));
  node->string = NULL;
  
  node->first_keyvaluepair = NULL;
  node->key = NULL;
  node->value = NULL;
  node->next_keyvaluepair = NULL;
  
  node->first_listitem = NULL;
  node->node = NULL;
  node->next_listitem = NULL;
  return node;
}

TypeNode* read_value(yaml_document_t *document_p, yaml_node_t *node)
{
	yaml_node_t *next_node_p;
  TypeNode *mynode;
  
	switch (node->type) {
		case YAML_NO_NODE:
			mynode = create_TypeNode();
      mynode->type = 4;
			break;
		case YAML_SCALAR_NODE:
      mynode = create_TypeNode();
      mynode->type = 3;
      mynode->string = (char*) malloc(STRING_LENGTH * sizeof(char));
      strcpy(mynode->string, node->data.scalar.value);
			break;
		case YAML_SEQUENCE_NODE:
      mynode = create_TypeNode();
      mynode->type = 2;
      mynode->first_listitem = create_TypeNode();
    
			yaml_node_item_t *i_node;
      TypeNode *listitem = mynode->first_listitem;
			for (i_node = node->data.sequence.items.start; i_node < node->data.sequence.items.top; i_node++) {
				next_node_p = yaml_document_get_node(document_p, *i_node);
				listitem->node = read_value(document_p, next_node_p);
        if (i_node < node->data.sequence.items.top - 1){
          listitem->next_listitem = create_TypeNode();
          listitem = listitem->next_listitem;
        }
      }
			break;
		case YAML_MAPPING_NODE:
      
			mynode = create_TypeNode();
      
      mynode->type = 1;
      mynode->first_keyvaluepair = create_TypeNode();
      
      TypeNode *keyvaluepair = mynode->first_keyvaluepair;
			yaml_node_pair_t *i_node_p;
			for (i_node_p = node->data.mapping.pairs.start; i_node_p < node->data.mapping.pairs.top; i_node_p++) {
				
        keyvaluepair->key = (char*) malloc(STRING_LENGTH * sizeof(char));
        next_node_p = yaml_document_get_node(document_p, i_node_p->key);
        strcpy(keyvaluepair->key, next_node_p->data.scalar.value);

				next_node_p = yaml_document_get_node(document_p, i_node_p->value);
				keyvaluepair->value = read_value(document_p, next_node_p);
        
        if (i_node_p < node->data.mapping.pairs.top - 1){
          keyvaluepair->next_keyvaluepair = create_TypeNode();
          keyvaluepair = keyvaluepair->next_keyvaluepair;
        }
			}
			break;
    }  
  return mynode;
}

void destroy(TypeNode *node)
{
  if (node->type == 1){
    TypeNode *pair;
    TypeNode *next;
    pair = node->first_keyvaluepair;    
    while (pair){
      next = pair->next_keyvaluepair;
      destroy(pair->value);
      free(pair->key);
      free(pair->value);
      free(pair);
      pair = next;
    }
    node->first_keyvaluepair = NULL;
  }
  else if (node->type == 2){
    TypeNode *item;
    TypeNode *next;
    item = node->first_listitem;
    while (item){
      next = item->next_listitem;
      destroy(item->node);
      free(item->node);
      free(item);
      item = next;
    }
    node->first_listitem = NULL;
  }
  else if (node->type == 3){
    free(node->string);
  }
  else if (node->type == 4){
    // nothing
  }
}

void set_error(const char *message, char *error){
  for (int i = 0; i < strlen(message); i++){
    error[i] = message[i];
  }
  for (int i = strlen(message); i< STRING_LENGTH; i++){
    error[i] = ' '; 
  }
}

TypeNode* LoadFile_c(const char *file_name, char *error)
{
	yaml_parser_t parser;
	yaml_document_t document;
  TypeNode* root;

	FILE *file = fopen(file_name, "rb");
  if (!file){
    set_error("File does not exist.", error);
    return NULL;
  }

	yaml_parser_initialize(&parser);
	yaml_parser_set_input_file(&parser, file);

	if (!yaml_parser_load(&parser, &document)) {
    set_error("Failed to load document.", error);
		return NULL;
	}

  root = read_value(&document, yaml_document_get_root_node(&document));
	yaml_document_delete(&document);
	yaml_parser_delete(&parser);
	fclose(file);
  
  set_error("", error);
  return root;
}

void DestroyNode(TypeNode *root)
{
  destroy(root);
  free(root);
  root = NULL;
}