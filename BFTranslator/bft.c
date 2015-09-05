//Brainfuck-to-BIN Translator for PIC Brainfuck Computer

//Instruction Decode as follows:
//000 +
//001 ,
//010 -
//011 .
//100 <
//101 ]
//110 >
//111 [
//0x08 EOF

#include <stdio.h>
#include <stdlib.h>

struct binData {
	char data;
	struct binData *next;
};

void printUsage(char *s);
struct binData *newBin(struct binData *prev);
FILE *in, *out;
int format = 0; //text or binary output

int main(int argc, char *argv[]) {
	in = stdin;
	out = stdout;
	if(argc > 1 && argc <= 6) {
		int i;
		for(i = 1; i < argc; i++) {
			if(!strcmp(argv[i], "-i")) {
				if(i+1 < argc) {
					in = fopen(argv[i+1], "r");
					if(!in) {
						fprintf(stderr, "Could not open %s\n", argv[i+1]);
						return 1;
					}
					i++;
				} else {
					printUsage(argv[0]);
					return 1;
				}
			} else if(!strcmp(argv[i], "-o")) {
				if(i+1 < argc) {
					out = fopen(argv[i+1], "wb");
					if(!out) {
						fprintf(stderr, "Could not open %s\n", argv[i+1]);
						return 1;
					}
					i++;
				} else {
					printUsage(argv[0]);
					return 1;
				}
			} else if(!strcmp(argv[i], "-b")) {
				format = 1;
			} else {
				printUsage(argv[0]);
				return 1;
			}
		}
	} else if(argc != 1) {
		printUsage(argv[0]);
		return 1;
	}
	
	//Parse the input and write the output
	char inst, temp;
	struct binData *start = NULL;
	struct binData *current = NULL;
	int instCount = 0;
	int byteCount = 0;
	int flag = 0;
	
	while(fscanf(in, "%c", &inst) != EOF) {
		switch(inst) {
			case '+':
				temp = 0x00;
				break;
			case '-':
				temp = 0x02;
				break;
			case '<':
				temp = 0x04;
				break;
			case '>':
				temp = 0x06;
				break;
			case '[':
				temp = 0x07;
				break;
			case ']':
				temp = 0x05;
				break;
			case '.':
				temp = 0x03;
				break;
			case ',':
				temp = 0x01;
				break;
			default:
				flag = 1;
		}
		if(!flag) {
			if(instCount%2 == 0) {
				current = newBin(current);
				if(!start)
					start = current;
				byteCount++;
				current->data = temp << 4;
			} else {
				current->data = current->data | temp;
			}
			instCount++;
		} else {
			flag = 0; //ignore other chars
		}
	}
	//append EOF instruction
	if(instCount%2 == 0) {
		current = newBin(current);
		if(!start)
			start = current;
		byteCount++;
		current->data = 0x80;
	} else {
		current->data = current->data | 0x08;
	}
	
	
	if(format == 0) {
		while(start) {
			fprintf(out, "0x%hhX", start->data);
			if(start->next) {
				fprintf(out, ",");
				start = start->next;
			} else {
				break;
			}
		}
		fprintf(out, "\n");
	} else if(format == 1) {
		while(start) {
			fwrite(&(start->data), 1, sizeof(char), out);
			if(start->next) {
				start = start->next;
			} else {
				break;
			}
		}
	}
	fprintf(stderr, "%d instructions, %d bytes used.\n", instCount+1, byteCount);
}

struct binData *newBin(struct binData *prev) {
	struct binData *bin;
	if(!(bin=malloc(sizeof(struct binData)))) {
		fprintf(stderr, "Error: Cannot allocate memory.\n");
		return NULL;
	}
	bin->next = NULL;
	if(prev)
		prev->next = bin;
	return bin;
}

void printUsage(char *s) {
	fprintf(stderr, "Usage: %s [-i input_file] [-o output_file] [-b]\n", s);
}

char swap(char c) {
	return ((c & 0xF0 >> 4) | (c & 0x0F << 4));
}