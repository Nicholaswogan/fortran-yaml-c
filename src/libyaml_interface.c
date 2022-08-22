#include <yaml.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

typedef struct TypeNode TypeNode;
struct TypeNode
{
  // Node
  int type;
  
  // Scalar
  int string_len;
  char *string;
  
  // Dictionary
  TypeNode *first_keyvaluepair;
  int key_len;
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
      mynode->string_len = strlen((char *)node->data.scalar.value);
      mynode->string = (char*) malloc((mynode->string_len+1) * sizeof(char)); // buffer is len+1 to accomodate the terminating null char
      strncpy(mynode->string, (char *)node->data.scalar.value, (mynode->string_len+1)); 
      break;
    case YAML_SEQUENCE_NODE:
      mynode = create_TypeNode();
      mynode->type = 2;

      if (node->data.sequence.items.top - node->data.sequence.items.start == 0) {
        // empty list
        mynode->first_listitem = NULL;
      } 
      else {
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
      }
      break;
    case YAML_MAPPING_NODE:

      mynode = create_TypeNode();
      mynode->type = 1;

      if (node->data.sequence.items.top - node->data.sequence.items.start == 0) {
        // empty dictionary
        mynode->first_keyvaluepair = NULL;
      }
      else {
        mynode->first_keyvaluepair = create_TypeNode();

        TypeNode *keyvaluepair = mynode->first_keyvaluepair;
        yaml_node_pair_t *i_node_p;
        for (i_node_p = node->data.mapping.pairs.start; i_node_p < node->data.mapping.pairs.top; i_node_p++) {
          
          next_node_p = yaml_document_get_node(document_p, i_node_p->key);

          keyvaluepair->key_len = strlen((char *)next_node_p->data.scalar.value);
          keyvaluepair->key = (char*) malloc((keyvaluepair->key_len+1) * sizeof(char)); // buffer is len+1 to accomodate the terminating null char
          strncpy(keyvaluepair->key, (char *)next_node_p->data.scalar.value, (keyvaluepair->key_len+1));

          next_node_p = yaml_document_get_node(document_p, i_node_p->value);
          keyvaluepair->value = read_value(document_p, next_node_p);

          if (i_node_p < node->data.mapping.pairs.top - 1){
            keyvaluepair->next_keyvaluepair = create_TypeNode();
            keyvaluepair = keyvaluepair->next_keyvaluepair;
          }
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

char* concat(const char *s1, const char *s2)
{
  char *result = malloc(strlen(s1) + strlen(s2) + 1); // +1 for the null-terminator
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

TypeNode* LoadFile_c(const char *file_name, int* err_len, char **error)
{
  yaml_parser_t parser;
  yaml_document_t document;
  TypeNode* root;

  FILE *file = fopen(file_name, "rb");
  if (!file){
    *error = concat("Tried to parse the following YAML file but it does not exist: ", file_name);
    *err_len = strlen(*error);
    return NULL;
  }

  if (!yaml_parser_initialize(&parser)){
    *error = concat("Failed to initalize yaml parser.", "");
    *err_len = strlen(*error);
    fclose(file);
    return NULL;
  }
  yaml_parser_set_input_file(&parser, file);

  if (!yaml_parser_load(&parser, &document)) {
    *error = concat("Failed to parse the following YAML file: ", file_name);
    *err_len = strlen(*error);
    yaml_parser_delete(&parser);
    fclose(file);
    return NULL;
  }

  root = read_value(&document, yaml_document_get_root_node(&document));
  yaml_document_delete(&document);
  yaml_parser_delete(&parser);
  fclose(file);

  // no errors.
  *err_len = 0;
  *error = NULL;
  return root;
}

void DestroyNode(TypeNode *root)
{
  destroy(root);
  free(root);
  root = NULL;
}

void DestroyChar(char *err)
{
  free(err);
}
