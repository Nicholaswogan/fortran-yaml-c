#include <yaml.h>
#include <stdio.h>
#include <assert.h>


int main()
{
  
  FILE *file;
  yaml_parser_t parser;
  yaml_document_t document;
  yaml_node_t *node;

  
  file = fopen("../test.yaml", "rb");
  assert(file);
  
  yaml_parser_initialize(&parser);
  yaml_parser_set_input_file(&parser, file);
  yaml_parser_load(&parser, &document);
  
  
  node = yaml_document_get_node(&document,2);
  
  printf("Node : %d\n", node->type);
  if(node->type == YAML_SCALAR_NODE) {
    printf("Scalar : %s\n", node->data.scalar.value);
  }
  else if(node->type == YAML_MAPPING_NODE){
    printf("HI %i\n",node->start_mark.index);
    printf("HI %i\n",node->end_mark.index);
    // printf("HI %i\n",*node->data.mapping.pairs.top);
  }


  yaml_document_delete(&document);
  
  yaml_parser_delete(&parser);
  fclose(file);
  return 0;
}