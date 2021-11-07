#include <yaml.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

typedef struct TypeNode TypeNode;

struct TypeNode
{
  // Node
  int nodetype;
  
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
  
  // Utlities
  // bool IsMap(){return type == 1;}
  // bool IsSequence(){return type == 2;}
  // bool IsScalar(){return type == 3;}
  // bool IsNull(){return type == 4;}
};



void printl_utf8(unsigned char *str, size_t length, FILE *stream)
{
	fwrite(str, 1, length, stream);
}

// doesn't print styles.
void print_yaml_node(yaml_document_t *document_p, yaml_node_t *node)
{
	static int x = 0;
	x++;
	int node_n = x;

	yaml_node_t *next_node_p;

	switch (node->type) {
		case YAML_NO_NODE:
			printf("Empty node(%d):\n", node_n);
			break;
		case YAML_SCALAR_NODE:
			printf("Scalar node(%d):\n", node_n);
			printl_utf8(node->data.scalar.value, node->data.scalar.length, stdout);
			puts("");
			break;
		case YAML_SEQUENCE_NODE:
			printf("Sequence node(%d):\n", node_n);
			yaml_node_item_t *i_node;
			for (i_node = node->data.sequence.items.start; i_node < node->data.sequence.items.top; i_node++) {
				next_node_p = yaml_document_get_node(document_p, *i_node);
				if (next_node_p)
					print_yaml_node(document_p, next_node_p);
			}
			break;
		case YAML_MAPPING_NODE:
			printf("Mapping node(%d):\n", node_n);

			yaml_node_pair_t *i_node_p;
			for (i_node_p = node->data.mapping.pairs.start; i_node_p < node->data.mapping.pairs.top; i_node_p++) {
				next_node_p = yaml_document_get_node(document_p, i_node_p->key);
				if (next_node_p) {
					puts("Key:");
					print_yaml_node(document_p, next_node_p);
				} else {
					fputs("Couldn't find next node\n", stderr);
					exit(1);
				}

				next_node_p = yaml_document_get_node(document_p, i_node_p->value);
				if (next_node_p) {
					puts("Value:");
					print_yaml_node(document_p, next_node_p);
				} else {
					fputs("Couldn't find next node\n", stderr);
					exit(1);
				}
			}
			break;
		default:
			fputs("Unknown node type\n", stderr);
			exit(1);
			break;
	}

	printf("END NODE(%d)\n", node_n);
}

void print_yaml_document(yaml_document_t *document_p)
{
	puts("NEW DOCUMENT");

	print_yaml_node(document_p, yaml_document_get_root_node(document_p));

	puts("END DOCUMENT");
}

int print_yaml(const char *file_name)
{
	yaml_parser_t parser;
	yaml_document_t document;
	int error = 0;

	printf("Loading '%s': \n", file_name);

	FILE *file = fopen(file_name, "rb");
	assert(file);

	assert(yaml_parser_initialize(&parser));

	yaml_parser_set_input_file(&parser, file);

	int done = 0;
	while (!done)
	{
		if (!yaml_parser_load(&parser, &document)) {
			fprintf(stderr, "Failed to load document in %s\n", file_name);
			error = 1;
			break;
		}

		done = (!yaml_document_get_root_node(&document));

		if (!done)
			print_yaml_document(&document);

		yaml_document_delete(&document);
	}

	yaml_parser_delete(&parser);

	assert(!fclose(file));

	return !error;
}


int main()
{
  
  int success = print_yaml("../test.yaml");
  
  
  return 0;
}