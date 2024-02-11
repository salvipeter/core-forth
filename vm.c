#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define cell int32_t
#define ucell uint32_t
#define udcell uint64_t
#define CELL_SIZE 4
#define MEMORY_SIZE 10000000
#define RESERVED 20
#define RSTACK_SIZE 100
#define TIB_SIZE 50
#define DSTACK_SIZE 100

#define TRUE -1
#define FALSE 0

udcell mask = ((udcell)1 << 8 * CELL_SIZE) - 1;

ucell HERE    =  0*CELL_SIZE;
ucell DICT    =  1*CELL_SIZE;
ucell MEMEND  =  2*CELL_SIZE;
ucell RSP     =  3*CELL_SIZE;
ucell TIB     =  4*CELL_SIZE;
ucell IN      =  5*CELL_SIZE;
ucell TIBSIZE =  6*CELL_SIZE;
ucell DSP     =  7*CELL_SIZE;
ucell MEMSIZE =  8*CELL_SIZE;
ucell STATE   =  9*CELL_SIZE;
ucell LATEST  = 10*CELL_SIZE;

char *memory;

void set(ucell a_addr, cell x) {
  *(cell *)(memory + a_addr) = x;
}

cell get(ucell a_addr) {
  return *(cell *)(memory + a_addr);
}

int depth(void) {
  return (get(MEMSIZE) - get(DSP)) / CELL_SIZE;
}

ucell find_entry(const char *word) {
  int len = strlen(word);
  for (ucell entry = get(DICT); entry != 0; entry = get(entry)) {
    if (len != ((unsigned char)memory[entry+CELL_SIZE] & 0x1f))
      continue;
    if (strncmp(&memory[entry+CELL_SIZE+1], word, len) == 0)
      return entry;
  }
  return FALSE;
}

ucell to_body(ucell entry) {
  int len = (unsigned char)memory[entry+CELL_SIZE] & 0x1f;
  entry += CELL_SIZE + 1 + len;
  if (entry % CELL_SIZE != 0)
    entry += CELL_SIZE - entry % CELL_SIZE;
  return entry;
}

int readline() {
  if (!fgets(&memory[get(TIB)], TIB_SIZE * CELL_SIZE, stdin))
    return FALSE;
  int len = strlen(&memory[get(TIB)]);
  set(TIBSIZE, len - 1);        /* -1 for omitting the newline char */
  return TRUE;
}

int fn_get(void) {
  if (depth() < 1) {
    printf(" stack underflow\n");
    return FALSE;
  }
  ucell index = get(get(DSP));
  if (index >= get(MEMSIZE)) {
    printf(" invalid memory access\n");
    return FALSE;
  }
  set(get(DSP), get(index));
  return TRUE;
}

int fn_set(void) {
  if (depth() < 2) {
    printf(" stack underflow\n");
    return FALSE;
  }
  ucell index = get(get(DSP)), x = get(get(DSP) + CELL_SIZE);
  if (index >= get(MEMSIZE)) {
    printf(" invalid memory access\n");
    return FALSE;
  }
  set(index, x);
  if (index == DSP) {
    if (x > get(MEMSIZE)) {
      printf(" stack underflow");
      return FALSE;
    }
  } else
    set(DSP, get(DSP) + 2 * CELL_SIZE);
  return TRUE;
}

int fn_less(void) {
  if (depth() < 1) {
    printf(" stack underflow\n");
    return FALSE;
  }
  set(get(DSP), get(get(DSP)) < 0 ? TRUE : FALSE);
  return TRUE;
}

int fn_add(void) {
  if (depth() < 2) {
    printf(" stack underflow\n");
    return FALSE;
  }
  cell x = get(get(DSP)), y = get(get(DSP) + CELL_SIZE);
  set(DSP, get(DSP) + CELL_SIZE);
  set(get(DSP), x + y);
  return TRUE;
}

int fn_mult(void) {
  if (depth() < 2) {
    printf(" stack underflow\n");
    return FALSE;
  }
  ucell dsp = get(DSP);
  ucell x = get(dsp), y = get(dsp + CELL_SIZE);
  udcell r = (udcell)x * (udcell)y;
  set(dsp + CELL_SIZE, r & mask);
  set(dsp, (r >> 8 * CELL_SIZE) & mask);
  return TRUE;
}

int fn_cells(void) {
  if (depth() < 1) {
    printf(" stack underflow\n");
    return FALSE;
  }
  cell x = get(get(DSP));
  set(get(DSP), CELL_SIZE * x);
  return TRUE;
}

int fn_divmod(void) {
  if (depth() < 2) {
    printf(" stack underflow\n");
    return FALSE;
  }
  ucell dsp = get(DSP);
  ucell x = get(dsp), y = get(dsp + CELL_SIZE), z = get(dsp + 2 * CELL_SIZE);
  udcell yz = ((udcell)y << 8 * CELL_SIZE) + (udcell)z;
  udcell q = yz / (udcell)x;
  ucell m = yz % (udcell)x;
  set(dsp + 2 * CELL_SIZE, m);
  set(dsp + CELL_SIZE, q & mask);
  set(DSP, dsp + CELL_SIZE);
  return TRUE;
}

int fn_nand(void) {
  if (depth() < 2) {
    printf(" stack underflow\n");
    return FALSE;
  }
  ucell x = get(get(DSP)), y = get(get(DSP) + CELL_SIZE);
  set(DSP, get(DSP) + CELL_SIZE);
  set(get(DSP), ~(x & y));
  return TRUE;
}

int fn_key(void) {
  /* Not implemented yet */
  char c = 42;                  /* dummy value */
  set(IN, get(IN) + 1);
  set(DSP, get(DSP) - CELL_SIZE);
  set(get(DSP), c);
  return TRUE;
}

int fn_readline(void) {
  if (!readline())
    return FALSE;
  set(IN, 0);
  return TRUE;
}

int fn_emit(void) {
  if (depth() < 1) {
    printf(" stack underflow\n");
    return FALSE;
  }
  cell x = get(get(DSP));
  printf("%c", (char)(x & 0xff));
  fflush(stdout);
  set(DSP, get(DSP) + CELL_SIZE);
  return TRUE;
}

/* Assumes that the name is on the same line */
int fn_colon(void) {
  char name[32];
  int pos;
  if (sscanf(&memory[get(TIB)] + get(IN), "%s%n", name, &pos) != 1) {
    return FALSE;
  }
  set(IN, get(IN) + pos + 1);

  ucell start = get(HERE), here = start, dict = get(DICT);
  int size = strlen(name);
  set(here, dict); here += CELL_SIZE;
  set(here, (unsigned char)size); here++;
  memcpy(memory + here, name, size);
  here += size;
  if (here % CELL_SIZE != 0)
    here += CELL_SIZE - here % CELL_SIZE;
  set(HERE, here);

  set(LATEST, start);
  set(STATE, 1);
  return TRUE;
}

int fn_semicolon(void) {
  static ucell exit_body = 0;
  if (!exit_body)
    exit_body = to_body(find_entry("EXIT"));
  set(get(HERE), exit_body);
  set(HERE, get(HERE) + CELL_SIZE);
  set(DICT, get(LATEST));
  set(STATE, 0);
  return TRUE;
}

typedef int(*sysfn)(void);
sysfn sys_functions[] = {
  fn_set, fn_add, fn_less, fn_colon, fn_semicolon, fn_get,
  fn_cells, fn_emit, NULL /* exit */, fn_key, fn_nand,
  fn_readline, fn_mult, fn_divmod
};

void add_dict_entry(const char *name, int immediate, int code) {
  ucell here = get(HERE), dict = get(DICT);
  int size = strlen(name);
  set(DICT, here);
  set(here, dict); here += CELL_SIZE;
  set(here, (unsigned char)size + (immediate ? 0x80 : 0)); here++;
  memcpy(memory + here, name, size);
  here += size;
  if (here % CELL_SIZE != 0)
    here += CELL_SIZE - here % CELL_SIZE;
  set(here, code); here += CELL_SIZE;
  set(HERE, here);
}

int eval(ucell entry) {
  if (get(STATE) && !(get(entry + CELL_SIZE) & 0x80)) {
    /* Compilation */
    set(get(HERE), to_body(entry));
    set(HERE, get(HERE) + CELL_SIZE);
  } else {
    /* Interpretation */
    ucell old_rsp = get(RSP);
    set(RSP, old_rsp - CELL_SIZE);
    set(get(RSP), 0);              /* dummy return address */
    entry = to_body(entry);
    while (entry) {
      cell index = get(entry);
      if (index < 0) {
        if (index == -9) {     /* EXIT */
          entry = get(get(RSP) + CELL_SIZE);
          set(RSP, get(RSP) + 2 * CELL_SIZE);
        } else if (index == -15) { /* number literal */
          set(DSP, get(DSP) - CELL_SIZE);
          set(get(DSP), get(entry + CELL_SIZE));
          entry += 2 * CELL_SIZE;
        } else {
          if (!sys_functions[-index-1]())
            return FALSE;
          entry = get(get(RSP));
          set(RSP, get(RSP) + CELL_SIZE);
        }
      } else {
        /* User word */
        set(RSP, get(RSP) - CELL_SIZE);
        set(get(RSP), entry + CELL_SIZE);
        entry = index;
      }
    }
  }
  return TRUE;
}

void cleanup(void) {
  set(RSP,   (MEMORY_SIZE - DSTACK_SIZE - TIB_SIZE) * CELL_SIZE);
  set(DSP,   MEMORY_SIZE * CELL_SIZE);
  set(STATE, FALSE);
}

int parse_number(const char *str, int radix, cell *n) {
  *n = 0;
  cell sign = 1;
  int k;
  if (*str == '-') {
    sign = -1;
    str++;
  }
  while (*str) {
    if (*str >= '0' && *str <= '9')
      k = *str - '0';
    else if ((*str >= 'a' && *str <= 'z') || (*str >= 'A' && *str <= 'Z'))
      k = 10 + toupper(*str) - 'A';
    else
      return FALSE;
    if (k >= radix)
      return FALSE;
    *n = *n * radix + k;
    str++;
  }
  if (sign < 0 && *n == 0)
    return FALSE;
  *n *= sign;
  return TRUE;
}

int main(int argc, char **argv) {
  /* Allocate memory */
  memory = (char *)malloc(MEMORY_SIZE * CELL_SIZE);

  /* Set up system variables */
  set(HERE,    RESERVED * CELL_SIZE);
  set(DICT,    0);
  set(MEMEND,  (MEMORY_SIZE - DSTACK_SIZE - TIB_SIZE - RSTACK_SIZE) * CELL_SIZE);
  set(RSP,     (MEMORY_SIZE - DSTACK_SIZE - TIB_SIZE) * CELL_SIZE);
  set(TIB,     (MEMORY_SIZE - DSTACK_SIZE - TIB_SIZE) * CELL_SIZE);
  set(DSP,     MEMORY_SIZE * CELL_SIZE);
  set(MEMSIZE, MEMORY_SIZE * CELL_SIZE);
  set(STATE,   FALSE);

  /* Set up system functions */
  add_dict_entry("!",         FALSE,  -1);
  add_dict_entry("+",         FALSE,  -2);
  add_dict_entry("0<",        FALSE,  -3);
  add_dict_entry(":",         FALSE,  -4);
  add_dict_entry(";",         TRUE,   -5);
  add_dict_entry("@",         FALSE,  -6);
  add_dict_entry("CELLS",     FALSE,  -7);
  add_dict_entry("EMIT",      FALSE,  -8);
  add_dict_entry("EXIT",      FALSE,  -9);
  add_dict_entry("KEY",       FALSE, -10);
  add_dict_entry("NAND",      FALSE, -11);
  add_dict_entry("READ-LINE", FALSE, -12);
  add_dict_entry("UM*",       FALSE, -13);
  add_dict_entry("UM/MOD",    FALSE, -14);

  /* Interpreter loop */
  while (TRUE) {
    if (!readline())
      break;
    set(IN, 0);
    while (TRUE) {
      char word[32];
      int pos;
      if (sscanf(&memory[get(TIB)] + get(IN), "%s%n", word, &pos) != 1)
        break;
      set(IN, get(IN) + pos + 1);
      cell number;
      if (parse_number(word, 10, &number)) {
        /* Number */
        if (get(STATE)) {
          /* Compilation */
          set(get(HERE), -15);
          set(HERE, get(HERE) + CELL_SIZE);
          set(get(HERE), number);
          set(HERE, get(HERE) + CELL_SIZE);
        } else {
          /* Interpretation */
          set(DSP, get(DSP) - CELL_SIZE);
          set(get(DSP), number);
        }
        continue;
      }
      /* Not a number */
      ucell entry;
      if (!(entry = find_entry(word))) {
        printf(" no such word: %s\n", word);
        cleanup();
        break;
      }
      if (!eval(entry)) {
        printf(" error while executing: %s\n", word);
        cleanup();
        break;
      }
    }
#ifdef DEBUG
    printf("[%d] Stack:", (get(TIB) - get(RSP)) / CELL_SIZE);
    for (int i = depth() - 1; i >= 0; --i)
      printf(" %d", get(get(DSP) + i * CELL_SIZE));
    printf("\n");
#endif
  }
}
