/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "ctf-parser.y" /* yacc.c:339  */

/*
 * ctf-parser.y
 *
 * Common Trace Format Metadata Grammar.
 *
 * Copyright 2010 - Mathieu Desnoyers <mathieu.desnoyers@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <glib.h>
#include <errno.h>
#include <inttypes.h>
#include <babeltrace/list.h>
#include <babeltrace/babeltrace-internal.h>
#include "ctf-scanner.h"
#include "ctf-parser.h"
#include "ctf-ast.h"
#include "objstack.h"

BT_HIDDEN
int yydebug;

/* Join two lists, put "add" at the end of "head".  */
static inline void
_bt_list_splice_tail (struct bt_list_head *add, struct bt_list_head *head)
{
	/* Do nothing if the list which gets added is empty.  */
	if (add != add->next) {
		add->next->prev = head->prev;
		add->prev->next = head;
		head->prev->next = add->next;
		head->prev = add->prev;
	}
}

BT_HIDDEN
int yyparse(struct ctf_scanner *scanner, yyscan_t yyscanner);
BT_HIDDEN
int yylex(union YYSTYPE *yyval, yyscan_t yyscanner);
BT_HIDDEN
int yylex_init_extra(struct ctf_scanner *scanner, yyscan_t * ptr_yy_globals);
BT_HIDDEN
int yylex_destroy(yyscan_t yyscanner);
BT_HIDDEN
void yyrestart(FILE * in_str, yyscan_t yyscanner);
BT_HIDDEN
int yyget_lineno(yyscan_t yyscanner);
BT_HIDDEN
char *yyget_text(yyscan_t yyscanner);

static const char *node_type_to_str[] = {
#define ENTRY(S)	[S] = #S,
	FOREACH_CTF_NODES(ENTRY)
#undef ENTRY
};

/*
 * Static node for out of memory errors. Only "type" is used. lineno is
 * always left at 0. The rest of the node content can be overwritten,
 * but is never used.
 */
static struct ctf_node error_node = {
	.type = NODE_ERROR,
};

BT_HIDDEN
const char *node_type(struct ctf_node *node)
{
	if (node->type < NR_NODE_TYPES)
		return node_type_to_str[node->type];
	else
		return NULL;
}

void setstring(struct ctf_scanner *scanner, YYSTYPE *lvalp, const char *src)
{
	lvalp->s = objstack_alloc(scanner->objstack, strlen(src) + 1);
	strcpy(lvalp->s, src);
}

static
int str_check(size_t str_len, size_t offset, size_t len)
{
	/* check overflow */
	if (offset + len < offset)
		return -1;
	if (offset + len > str_len)
		return -1;
	return 0;
}

static
int bt_isodigit(int c)
{
	switch (c) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		return 1;
	default:
		return 0;
	}
}

static
int parse_base_sequence(const char *src, size_t len, size_t pos,
		char *buffer, size_t *buf_len, int base)
{
	const size_t max_char = 3;
	int nr_char = 0;

	while (!str_check(len, pos, 1) && nr_char < max_char) {
		char c = src[pos++];

		if (base == 8) {
			if (bt_isodigit(c))
				buffer[nr_char++] = c;
			else
				break;
		} else if (base == 16) {
			if (isxdigit(c))
				buffer[nr_char++] = c;
			else
				break;

		} else {
			/* Unsupported base */
			return -1;
		}
	}
	assert(nr_char > 0);
	buffer[nr_char] = '\0';
	*buf_len = nr_char;
	return 0;
}

static
int import_basic_string(struct ctf_scanner *scanner, YYSTYPE *lvalp,
		size_t len, const char *src, char delim)
{
	size_t pos = 0, dpos = 0;

	if (str_check(len, pos, 1))
		return -1;
	if (src[pos++] != delim)
		return -1;

	while (src[pos] != delim) {
		char c;

		if (str_check(len, pos, 1))
			return -1;
		c = src[pos++];
		if (c == '\\') {
			if (str_check(len, pos, 1))
				return -1;
			c = src[pos++];

			switch (c) {
			case 'a':
				c = '\a';
				break;
			case 'b':
				c = '\b';
				break;
			case 'f':
				c = '\f';
				break;
			case 'n':
				c = '\n';
				break;
			case 'r':
				c = '\r';
				break;
			case 't':
				c = '\t';
				break;
			case 'v':
				c = '\v';
				break;
			case '\\':
				c = '\\';
				break;
			case '\'':
				c = '\'';
				break;
			case '\"':
				c = '\"';
				break;
			case '?':
				c = '?';
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			{
				char oct_buffer[4];
				size_t oct_len;

				if (parse_base_sequence(src, len, pos - 1,
						oct_buffer, &oct_len, 8))
					return -1;
				c = strtoul(&oct_buffer[0], NULL, 8);
				pos += oct_len - 1;
				break;
			}
			case 'x':
			{
				char hex_buffer[4];
				size_t hex_len;

				if (parse_base_sequence(src, len, pos,
						hex_buffer, &hex_len, 16))
					return -1;
				c = strtoul(&hex_buffer[0], NULL, 16);
				pos += hex_len;
				break;
			}
			default:
				return -1;
			}
		}
		if (str_check(len, dpos, 1))
			return -1;
		lvalp->s[dpos++] = c;
	}

	if (str_check(len, dpos, 1))
		return -1;
	lvalp->s[dpos++] = '\0';

	if (str_check(len, pos, 1))
		return -1;
	if (src[pos++] != delim)
		return -1;

	if (str_check(len, pos, 1))
		return -1;
	if (src[pos] != '\0')
		return -1;
	return 0;
}

int import_string(struct ctf_scanner *scanner, YYSTYPE *lvalp,
		const char *src, char delim)
{
	size_t len;

	len = strlen(src) + 1;
	lvalp->s = objstack_alloc(scanner->objstack, len);
	if (src[0] == 'L') {
		// TODO: import wide string
		printfl_error(yyget_lineno(scanner),
			"Wide string not supported yet.");
		return -1;
	} else {
		return import_basic_string(scanner, lvalp, len, src, delim);
	}
}

static void init_scope(struct ctf_scanner_scope *scope,
		       struct ctf_scanner_scope *parent)
{
	scope->parent = parent;
	scope->types = g_hash_table_new_full(g_str_hash, g_str_equal,
					     NULL, NULL);
}

static void finalize_scope(struct ctf_scanner_scope *scope)
{
	g_hash_table_destroy(scope->types);
}

static void push_scope(struct ctf_scanner *scanner)
{
	struct ctf_scanner_scope *ns;

	printf_debug("push scope\n");
	ns = malloc(sizeof(struct ctf_scanner_scope));
	init_scope(ns, scanner->cs);
	scanner->cs = ns;
}

static void pop_scope(struct ctf_scanner *scanner)
{
	struct ctf_scanner_scope *os;

	printf_debug("pop scope\n");
	os = scanner->cs;
	scanner->cs = os->parent;
	finalize_scope(os);
	free(os);
}

static int lookup_type(struct ctf_scanner_scope *s, const char *id)
{
	int ret;

	ret = (int) (long) g_hash_table_lookup(s->types, id);
	printf_debug("lookup %p %s %d\n", s, id, ret);
	return ret;
}

BT_HIDDEN
int is_type(struct ctf_scanner *scanner, const char *id)
{
	struct ctf_scanner_scope *it;
	int ret = 0;

	for (it = scanner->cs; it != NULL; it = it->parent) {
		if (lookup_type(it, id)) {
			ret = 1;
			break;
		}
	}
	printf_debug("is type %s %d\n", id, ret);
	return ret;
}

static void add_type(struct ctf_scanner *scanner, char *id)
{
	printf_debug("add type %s\n", id);
	if (lookup_type(scanner->cs, id))
		return;
	g_hash_table_insert(scanner->cs->types, id, id);
}

static struct ctf_node *make_node(struct ctf_scanner *scanner,
				  enum node_type type)
{
	struct ctf_node *node;

	node = objstack_alloc(scanner->objstack, sizeof(*node));
	if (!node) {
		printfl_fatal(yyget_lineno(scanner->scanner), "out of memory");
		return &error_node;
	}
	node->type = type;
	node->lineno = yyget_lineno(scanner->scanner);
	BT_INIT_LIST_HEAD(&node->tmp_head);
	bt_list_add(&node->siblings, &node->tmp_head);

	switch (type) {
	case NODE_ROOT:
		node->type = NODE_ERROR;
		printfn_fatal(node, "trying to create root node");
		break;

	case NODE_EVENT:
		BT_INIT_LIST_HEAD(&node->u.event.declaration_list);
		break;
	case NODE_STREAM:
		BT_INIT_LIST_HEAD(&node->u.stream.declaration_list);
		break;
	case NODE_ENV:
		BT_INIT_LIST_HEAD(&node->u.env.declaration_list);
		break;
	case NODE_TRACE:
		BT_INIT_LIST_HEAD(&node->u.trace.declaration_list);
		break;
	case NODE_CLOCK:
		BT_INIT_LIST_HEAD(&node->u.clock.declaration_list);
		break;
	case NODE_CALLSITE:
		BT_INIT_LIST_HEAD(&node->u.callsite.declaration_list);
		break;

	case NODE_CTF_EXPRESSION:
		BT_INIT_LIST_HEAD(&node->u.ctf_expression.left);
		BT_INIT_LIST_HEAD(&node->u.ctf_expression.right);
		break;
	case NODE_UNARY_EXPRESSION:
		break;

	case NODE_TYPEDEF:
		BT_INIT_LIST_HEAD(&node->u._typedef.type_declarators);
		break;
	case NODE_TYPEALIAS_TARGET:
		BT_INIT_LIST_HEAD(&node->u.typealias_target.type_declarators);
		break;
	case NODE_TYPEALIAS_ALIAS:
		BT_INIT_LIST_HEAD(&node->u.typealias_alias.type_declarators);
		break;
	case NODE_TYPEALIAS:
		break;

	case NODE_TYPE_SPECIFIER:
		break;
	case NODE_TYPE_SPECIFIER_LIST:
		BT_INIT_LIST_HEAD(&node->u.type_specifier_list.head);
		break;
	case NODE_POINTER:
		break;
	case NODE_TYPE_DECLARATOR:
		BT_INIT_LIST_HEAD(&node->u.type_declarator.pointers);
		break;

	case NODE_FLOATING_POINT:
		BT_INIT_LIST_HEAD(&node->u.floating_point.expressions);
		break;
	case NODE_INTEGER:
		BT_INIT_LIST_HEAD(&node->u.integer.expressions);
		break;
	case NODE_STRING:
		BT_INIT_LIST_HEAD(&node->u.string.expressions);
		break;
	case NODE_ENUMERATOR:
		BT_INIT_LIST_HEAD(&node->u.enumerator.values);
		break;
	case NODE_ENUM:
		BT_INIT_LIST_HEAD(&node->u._enum.enumerator_list);
		break;
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
		BT_INIT_LIST_HEAD(&node->u.struct_or_variant_declaration.type_declarators);
		break;
	case NODE_VARIANT:
		BT_INIT_LIST_HEAD(&node->u.variant.declaration_list);
		break;
	case NODE_STRUCT:
		BT_INIT_LIST_HEAD(&node->u._struct.declaration_list);
		BT_INIT_LIST_HEAD(&node->u._struct.min_align);
		break;

	case NODE_UNKNOWN:
	default:
		node->type = NODE_ERROR;
		printfn_fatal(node, "unknown node type '%d'", (int) type);
		break;
	}

	return node;
}

static int reparent_ctf_expression(struct ctf_node *node,
				   struct ctf_node *parent)
{
	switch (parent->type) {
	case NODE_EVENT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.event.declaration_list);
		break;
	case NODE_STREAM:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.stream.declaration_list);
		break;
	case NODE_ENV:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.env.declaration_list);
		break;
	case NODE_TRACE:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.trace.declaration_list);
		break;
	case NODE_CLOCK:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.clock.declaration_list);
		break;
	case NODE_CALLSITE:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.callsite.declaration_list);
		break;
	case NODE_FLOATING_POINT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.floating_point.expressions);
		break;
	case NODE_INTEGER:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.integer.expressions);
		break;
	case NODE_STRING:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.string.expressions);
		break;

	case NODE_ROOT:
	case NODE_CTF_EXPRESSION:
	case NODE_TYPEDEF:
	case NODE_TYPEALIAS_TARGET:
	case NODE_TYPEALIAS_ALIAS:
	case NODE_TYPEALIAS:
	case NODE_TYPE_SPECIFIER:
	case NODE_TYPE_SPECIFIER_LIST:
	case NODE_POINTER:
	case NODE_TYPE_DECLARATOR:
	case NODE_ENUMERATOR:
	case NODE_ENUM:
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
	case NODE_VARIANT:
	case NODE_STRUCT:
	case NODE_UNARY_EXPRESSION:
		return -EPERM;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type '%d'", (int) parent->type);
		return -EINVAL;
	}
	return 0;
}

static int reparent_typedef(struct ctf_node *node, struct ctf_node *parent)
{
	switch (parent->type) {
	case NODE_ROOT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.root.declaration_list);
		break;
	case NODE_EVENT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.event.declaration_list);
		break;
	case NODE_STREAM:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.stream.declaration_list);
		break;
	case NODE_ENV:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.env.declaration_list);
		break;
	case NODE_TRACE:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.trace.declaration_list);
		break;
	case NODE_CLOCK:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.clock.declaration_list);
		break;
	case NODE_CALLSITE:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.callsite.declaration_list);
		break;
	case NODE_VARIANT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.variant.declaration_list);
		break;
	case NODE_STRUCT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u._struct.declaration_list);
		break;

	case NODE_FLOATING_POINT:
	case NODE_INTEGER:
	case NODE_STRING:
	case NODE_CTF_EXPRESSION:
	case NODE_TYPEDEF:
	case NODE_TYPEALIAS_TARGET:
	case NODE_TYPEALIAS_ALIAS:
	case NODE_TYPEALIAS:
	case NODE_TYPE_SPECIFIER:
	case NODE_TYPE_SPECIFIER_LIST:
	case NODE_POINTER:
	case NODE_TYPE_DECLARATOR:
	case NODE_ENUMERATOR:
	case NODE_ENUM:
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
	case NODE_UNARY_EXPRESSION:
		return -EPERM;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type %d", parent->type);
		return -EINVAL;
	}
	return 0;
}

static int reparent_typealias(struct ctf_node *node, struct ctf_node *parent)
{
	switch (parent->type) {
	case NODE_ROOT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.root.declaration_list);
		break;
	case NODE_EVENT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.event.declaration_list);
		break;
	case NODE_STREAM:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.stream.declaration_list);
		break;
	case NODE_ENV:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.env.declaration_list);
		break;
	case NODE_TRACE:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.trace.declaration_list);
		break;
	case NODE_CLOCK:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.clock.declaration_list);
		break;
	case NODE_CALLSITE:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.callsite.declaration_list);
		break;
	case NODE_VARIANT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.variant.declaration_list);
		break;
	case NODE_STRUCT:
		_bt_list_splice_tail(&node->tmp_head, &parent->u._struct.declaration_list);
		break;

	case NODE_FLOATING_POINT:
	case NODE_INTEGER:
	case NODE_STRING:
	case NODE_CTF_EXPRESSION:
	case NODE_TYPEDEF:
	case NODE_TYPEALIAS_TARGET:
	case NODE_TYPEALIAS_ALIAS:
	case NODE_TYPEALIAS:
	case NODE_TYPE_SPECIFIER:
	case NODE_TYPE_SPECIFIER_LIST:
	case NODE_POINTER:
	case NODE_TYPE_DECLARATOR:
	case NODE_ENUMERATOR:
	case NODE_ENUM:
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
	case NODE_UNARY_EXPRESSION:
		return -EPERM;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type '%d'", (int) parent->type);
		return -EINVAL;
	}
	return 0;
}

static int reparent_type_specifier(struct ctf_node *node,
				   struct ctf_node *parent)
{
	switch (parent->type) {
	case NODE_TYPE_SPECIFIER_LIST:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.type_specifier_list.head);
		break;

	case NODE_TYPE_SPECIFIER:
	case NODE_EVENT:
	case NODE_STREAM:
	case NODE_ENV:
	case NODE_TRACE:
	case NODE_CLOCK:
	case NODE_CALLSITE:
	case NODE_VARIANT:
	case NODE_STRUCT:
	case NODE_TYPEDEF:
	case NODE_TYPEALIAS_TARGET:
	case NODE_TYPEALIAS_ALIAS:
	case NODE_TYPE_DECLARATOR:
	case NODE_ENUM:
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
	case NODE_TYPEALIAS:
	case NODE_FLOATING_POINT:
	case NODE_INTEGER:
	case NODE_STRING:
	case NODE_CTF_EXPRESSION:
	case NODE_POINTER:
	case NODE_ENUMERATOR:
	case NODE_UNARY_EXPRESSION:
		return -EPERM;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type '%d'", (int) parent->type);
		return -EINVAL;
	}
	return 0;
}

static int reparent_type_specifier_list(struct ctf_node *node,
					struct ctf_node *parent)
{
	switch (parent->type) {
	case NODE_ROOT:
		bt_list_add_tail(&node->siblings, &parent->u.root.declaration_list);
		break;
	case NODE_EVENT:
		bt_list_add_tail(&node->siblings, &parent->u.event.declaration_list);
		break;
	case NODE_STREAM:
		bt_list_add_tail(&node->siblings, &parent->u.stream.declaration_list);
		break;
	case NODE_ENV:
		bt_list_add_tail(&node->siblings, &parent->u.env.declaration_list);
		break;
	case NODE_TRACE:
		bt_list_add_tail(&node->siblings, &parent->u.trace.declaration_list);
		break;
	case NODE_CLOCK:
		bt_list_add_tail(&node->siblings, &parent->u.clock.declaration_list);
		break;
	case NODE_CALLSITE:
		bt_list_add_tail(&node->siblings, &parent->u.callsite.declaration_list);
		break;
	case NODE_VARIANT:
		bt_list_add_tail(&node->siblings, &parent->u.variant.declaration_list);
		break;
	case NODE_STRUCT:
		bt_list_add_tail(&node->siblings, &parent->u._struct.declaration_list);
		break;
	case NODE_TYPEDEF:
		parent->u._typedef.type_specifier_list = node;
		break;
	case NODE_TYPEALIAS_TARGET:
		parent->u.typealias_target.type_specifier_list = node;
		break;
	case NODE_TYPEALIAS_ALIAS:
		parent->u.typealias_alias.type_specifier_list = node;
		break;
	case NODE_ENUM:
		parent->u._enum.container_type = node;
		break;
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
		parent->u.struct_or_variant_declaration.type_specifier_list = node;
		break;
	case NODE_TYPE_DECLARATOR:
	case NODE_TYPE_SPECIFIER:
	case NODE_TYPEALIAS:
	case NODE_FLOATING_POINT:
	case NODE_INTEGER:
	case NODE_STRING:
	case NODE_CTF_EXPRESSION:
	case NODE_POINTER:
	case NODE_ENUMERATOR:
	case NODE_UNARY_EXPRESSION:
		return -EPERM;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type '%d'", (int) parent->type);
		return -EINVAL;
	}
	return 0;
}

static int reparent_type_declarator(struct ctf_node *node,
				    struct ctf_node *parent)
{
	switch (parent->type) {
	case NODE_TYPE_DECLARATOR:
		parent->u.type_declarator.type = TYPEDEC_NESTED;
		parent->u.type_declarator.u.nested.type_declarator = node;
		break;
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.struct_or_variant_declaration.type_declarators);
		break;
	case NODE_TYPEDEF:
		_bt_list_splice_tail(&node->tmp_head, &parent->u._typedef.type_declarators);
		break;
	case NODE_TYPEALIAS_TARGET:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.typealias_target.type_declarators);
		break;
	case NODE_TYPEALIAS_ALIAS:
		_bt_list_splice_tail(&node->tmp_head, &parent->u.typealias_alias.type_declarators);
		break;

	case NODE_ROOT:
	case NODE_EVENT:
	case NODE_STREAM:
	case NODE_ENV:
	case NODE_TRACE:
	case NODE_CLOCK:
	case NODE_CALLSITE:
	case NODE_VARIANT:
	case NODE_STRUCT:
	case NODE_TYPEALIAS:
	case NODE_ENUM:
	case NODE_FLOATING_POINT:
	case NODE_INTEGER:
	case NODE_STRING:
	case NODE_CTF_EXPRESSION:
	case NODE_TYPE_SPECIFIER:
	case NODE_TYPE_SPECIFIER_LIST:
	case NODE_POINTER:
	case NODE_ENUMERATOR:
	case NODE_UNARY_EXPRESSION:
		return -EPERM;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type '%d'", (int) parent->type);
		return -EINVAL;
	}
	return 0;
}

/*
 * set_parent_node
 *
 * Link node to parent. Returns 0 on success, -EPERM if it is not permitted to
 * create the link declared by the input, -ENOENT if node or parent is NULL,
 * -EINVAL if there is an internal structure problem.
 */
static int set_parent_node(struct ctf_node *node,
			 struct ctf_node *parent)
{
	if (!node || !parent)
		return -ENOENT;

	/* Note: Linking to parent will be done only by an external visitor */

	switch (node->type) {
	case NODE_ROOT:
		printfn_fatal(node, "trying to reparent root node");
		return -EINVAL;

	case NODE_EVENT:
		if (parent->type == NODE_ROOT) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.root.event);
		} else {
			return -EPERM;
		}
		break;
	case NODE_STREAM:
		if (parent->type == NODE_ROOT) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.root.stream);
		} else {
			return -EPERM;
		}
		break;
	case NODE_ENV:
		if (parent->type == NODE_ROOT) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.root.env);
		} else {
			return -EPERM;
		}
		break;
	case NODE_TRACE:
		if (parent->type == NODE_ROOT) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.root.trace);
		} else {
			return -EPERM;
		}
		break;
	case NODE_CLOCK:
		if (parent->type == NODE_ROOT) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.root.clock);
		} else {
			return -EPERM;
		}
		break;
	case NODE_CALLSITE:
		if (parent->type == NODE_ROOT) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.root.callsite);
		} else {
			return -EPERM;
		}
		break;

	case NODE_CTF_EXPRESSION:
		return reparent_ctf_expression(node, parent);
	case NODE_UNARY_EXPRESSION:
		if (parent->type == NODE_TYPE_DECLARATOR)
			parent->u.type_declarator.bitfield_len = node;
		else
			return -EPERM;
		break;

	case NODE_TYPEDEF:
		return reparent_typedef(node, parent);
	case NODE_TYPEALIAS_TARGET:
		if (parent->type == NODE_TYPEALIAS)
			parent->u.typealias.target = node;
		else
			return -EINVAL;
	case NODE_TYPEALIAS_ALIAS:
		if (parent->type == NODE_TYPEALIAS)
			parent->u.typealias.alias = node;
		else
			return -EINVAL;
	case NODE_TYPEALIAS:
		return reparent_typealias(node, parent);

	case NODE_POINTER:
		if (parent->type == NODE_TYPE_DECLARATOR) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u.type_declarator.pointers);
		} else
			return -EPERM;
		break;
	case NODE_TYPE_DECLARATOR:
		return reparent_type_declarator(node, parent);

	case NODE_TYPE_SPECIFIER_LIST:
		return reparent_type_specifier_list(node, parent);

	case NODE_TYPE_SPECIFIER:
		return reparent_type_specifier(node, parent);

	case NODE_FLOATING_POINT:
	case NODE_INTEGER:
	case NODE_STRING:
	case NODE_ENUM:
	case NODE_VARIANT:
	case NODE_STRUCT:
		return -EINVAL;	/* Dealt with internally within grammar */

	case NODE_ENUMERATOR:
		if (parent->type == NODE_ENUM) {
			_bt_list_splice_tail(&node->tmp_head, &parent->u._enum.enumerator_list);
		} else {
			return -EPERM;
		}
		break;
	case NODE_STRUCT_OR_VARIANT_DECLARATION:
		switch (parent->type) {
		case NODE_STRUCT:
			_bt_list_splice_tail(&node->tmp_head, &parent->u._struct.declaration_list);
			break;
		case NODE_VARIANT:
			_bt_list_splice_tail(&node->tmp_head, &parent->u.variant.declaration_list);
			break;
		default:
			return -EINVAL;
		}
		break;

	case NODE_UNKNOWN:
	default:
		printfn_fatal(node, "unknown node type '%d'", (int) parent->type);
		return -EINVAL;
	}
	return 0;
}

BT_HIDDEN
void yyerror(struct ctf_scanner *scanner, yyscan_t yyscanner, const char *str)
{
	printfl_error(yyget_lineno(scanner->scanner),
		"token \"%s\": %s\n",
		yyget_text(scanner->scanner), str);
}
 
BT_HIDDEN
int yywrap(void)
{
	return 1;
} 

#define reparent_error(scanner, str)				\
do {								\
	yyerror(scanner, scanner->scanner, YY_("reparent_error: " str)); \
	YYERROR;						\
} while (0)

static struct ctf_ast *ctf_ast_alloc(struct ctf_scanner *scanner)
{
	struct ctf_ast *ast;

	ast = objstack_alloc(scanner->objstack, sizeof(*ast));
	if (!ast)
		return NULL;
	ast->root.type = NODE_ROOT;
	BT_INIT_LIST_HEAD(&ast->root.tmp_head);
	BT_INIT_LIST_HEAD(&ast->root.u.root.declaration_list);
	BT_INIT_LIST_HEAD(&ast->root.u.root.trace);
	BT_INIT_LIST_HEAD(&ast->root.u.root.env);
	BT_INIT_LIST_HEAD(&ast->root.u.root.stream);
	BT_INIT_LIST_HEAD(&ast->root.u.root.event);
	BT_INIT_LIST_HEAD(&ast->root.u.root.clock);
	BT_INIT_LIST_HEAD(&ast->root.u.root.callsite);
	return ast;
}

int ctf_scanner_append_ast(struct ctf_scanner *scanner, FILE *input)
{
	/* Start processing new stream */
	yyrestart(input, scanner->scanner);
	if (yydebug)
		fprintf(stdout, "Scanner input is a%s.\n",
			isatty(fileno(input)) ? "n interactive tty" :
						" noninteractive file");
	return yyparse(scanner, scanner->scanner);
}

struct ctf_scanner *ctf_scanner_alloc(void)
{
	struct ctf_scanner *scanner;
	int ret;

	yydebug = babeltrace_debug;

	scanner = malloc(sizeof(*scanner));
	if (!scanner)
		return NULL;
	memset(scanner, 0, sizeof(*scanner));
	ret = yylex_init_extra(scanner, &scanner->scanner);
	if (ret) {
		printf_fatal("yylex_init error");
		goto cleanup_scanner;
	}
	scanner->objstack = objstack_create();
	if (!scanner->objstack)
		goto cleanup_lexer;
	scanner->ast = ctf_ast_alloc(scanner);
	if (!scanner->ast)
		goto cleanup_objstack;
	init_scope(&scanner->root_scope, NULL);
	scanner->cs = &scanner->root_scope;

	return scanner;

cleanup_objstack:
	objstack_destroy(scanner->objstack);
cleanup_lexer:
	ret = yylex_destroy(scanner->scanner);
	if (!ret)
		printf_fatal("yylex_destroy error");
cleanup_scanner:
	free(scanner);
	return NULL;
}

void ctf_scanner_free(struct ctf_scanner *scanner)
{
	int ret;

	if (!scanner)
		return;
	finalize_scope(&scanner->root_scope);
	objstack_destroy(scanner->objstack);
	ret = yylex_destroy(scanner->scanner);
	if (ret)
		printf_error("yylex_destroy error");
	free(scanner);
}


#line 1105 "ctf-parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    INTEGER_LITERAL = 258,
    STRING_LITERAL = 259,
    CHARACTER_LITERAL = 260,
    LSBRAC = 261,
    RSBRAC = 262,
    LPAREN = 263,
    RPAREN = 264,
    LBRAC = 265,
    RBRAC = 266,
    RARROW = 267,
    STAR = 268,
    PLUS = 269,
    MINUS = 270,
    LT = 271,
    GT = 272,
    TYPEASSIGN = 273,
    COLON = 274,
    SEMICOLON = 275,
    DOTDOTDOT = 276,
    DOT = 277,
    EQUAL = 278,
    COMMA = 279,
    CONST = 280,
    CHAR = 281,
    DOUBLE = 282,
    ENUM = 283,
    ENV = 284,
    EVENT = 285,
    FLOATING_POINT = 286,
    FLOAT = 287,
    INTEGER = 288,
    INT = 289,
    LONG = 290,
    SHORT = 291,
    SIGNED = 292,
    STREAM = 293,
    STRING = 294,
    STRUCT = 295,
    TRACE = 296,
    CALLSITE = 297,
    CLOCK = 298,
    TYPEALIAS = 299,
    TYPEDEF = 300,
    UNSIGNED = 301,
    VARIANT = 302,
    VOID = 303,
    _BOOL = 304,
    _COMPLEX = 305,
    _IMAGINARY = 306,
    TOK_ALIGN = 307,
    IDENTIFIER = 308,
    ID_TYPE = 309,
    ERROR = 310
  };
#endif
/* Tokens.  */
#define INTEGER_LITERAL 258
#define STRING_LITERAL 259
#define CHARACTER_LITERAL 260
#define LSBRAC 261
#define RSBRAC 262
#define LPAREN 263
#define RPAREN 264
#define LBRAC 265
#define RBRAC 266
#define RARROW 267
#define STAR 268
#define PLUS 269
#define MINUS 270
#define LT 271
#define GT 272
#define TYPEASSIGN 273
#define COLON 274
#define SEMICOLON 275
#define DOTDOTDOT 276
#define DOT 277
#define EQUAL 278
#define COMMA 279
#define CONST 280
#define CHAR 281
#define DOUBLE 282
#define ENUM 283
#define ENV 284
#define EVENT 285
#define FLOATING_POINT 286
#define FLOAT 287
#define INTEGER 288
#define INT 289
#define LONG 290
#define SHORT 291
#define SIGNED 292
#define STREAM 293
#define STRING 294
#define STRUCT 295
#define TRACE 296
#define CALLSITE 297
#define CLOCK 298
#define TYPEALIAS 299
#define TYPEDEF 300
#define UNSIGNED 301
#define VARIANT 302
#define VOID 303
#define _BOOL 304
#define _COMPLEX 305
#define _IMAGINARY 306
#define TOK_ALIGN 307
#define IDENTIFIER 308
#define ID_TYPE 309
#define ERROR 310

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 1060 "ctf-parser.y" /* yacc.c:355  */

	long long ll;
	unsigned long long ull;
	char c;
	char *s;
	struct ctf_node *n;

#line 1263 "ctf-parser.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (struct ctf_scanner *scanner, yyscan_t yyscanner);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 1277 "ctf-parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  72
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2168

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  58
/* YYNRULES -- Number of rules.  */
#define YYNRULES  232
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  443

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   310

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1115,  1115,  1120,  1128,  1130,  1132,  1134,  1136,  1138,
    1140,  1142,  1144,  1146,  1148,  1150,  1152,  1154,  1156,  1158,
    1160,  1162,  1164,  1166,  1168,  1170,  1172,  1174,  1176,  1178,
    1180,  1188,  1194,  1200,  1206,  1212,  1218,  1224,  1228,  1236,
    1245,  1254,  1263,  1275,  1277,  1285,  1302,  1308,  1315,  1317,
    1319,  1321,  1323,  1325,  1327,  1329,  1340,  1350,  1360,  1381,
    1385,  1394,  1399,  1405,  1409,  1418,  1423,  1428,  1432,  1441,
    1446,  1451,  1455,  1464,  1469,  1474,  1478,  1487,  1492,  1497,
    1501,  1510,  1515,  1520,  1529,  1537,  1546,  1554,  1563,  1571,
    1580,  1588,  1590,  1598,  1603,  1608,  1613,  1618,  1623,  1628,
    1633,  1639,  1645,  1656,  1661,  1666,  1671,  1676,  1681,  1686,
    1691,  1696,  1701,  1706,  1711,  1716,  1722,  1728,  1736,  1742,
    1750,  1756,  1762,  1770,  1776,  1782,  1791,  1798,  1806,  1814,
    1820,  1826,  1834,  1843,  1855,  1860,  1865,  1872,  1880,  1888,
    1896,  1905,  1912,  1921,  1928,  1936,  1945,  1952,  1961,  1971,
    1976,  1981,  1987,  1994,  2001,  2009,  2016,  2024,  2030,  2037,
    2044,  2052,  2058,  2065,  2073,  2083,  2084,  2097,  2107,  2118,
    2128,  2138,  2159,  2168,  2176,  2187,  2196,  2201,  2215,  2217,
    2225,  2227,  2229,  2238,  2240,  2248,  2253,  2258,  2263,  2268,
    2274,  2280,  2286,  2295,  2297,  2305,  2307,  2316,  2321,  2327,
    2333,  2341,  2351,  2353,  2361,  2363,  2372,  2377,  2383,  2391,
    2401,  2403,  2411,  2417,  2423,  2434,  2436,  2444,  2451,  2457,
    2468,  2472,  2477,  2487,  2488,  2494,  2496,  2504,  2516,  2528,
    2539,  2549,  2559
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTEGER_LITERAL", "STRING_LITERAL",
  "CHARACTER_LITERAL", "LSBRAC", "RSBRAC", "LPAREN", "RPAREN", "LBRAC",
  "RBRAC", "RARROW", "STAR", "PLUS", "MINUS", "LT", "GT", "TYPEASSIGN",
  "COLON", "SEMICOLON", "DOTDOTDOT", "DOT", "EQUAL", "COMMA", "CONST",
  "CHAR", "DOUBLE", "ENUM", "ENV", "EVENT", "FLOATING_POINT", "FLOAT",
  "INTEGER", "INT", "LONG", "SHORT", "SIGNED", "STREAM", "STRING",
  "STRUCT", "TRACE", "CALLSITE", "CLOCK", "TYPEALIAS", "TYPEDEF",
  "UNSIGNED", "VARIANT", "VOID", "_BOOL", "_COMPLEX", "_IMAGINARY",
  "TOK_ALIGN", "IDENTIFIER", "ID_TYPE", "ERROR", "$accept", "file",
  "keywords", "postfix_expression", "unary_expression",
  "unary_expression_or_range", "declaration", "event_declaration",
  "event_declaration_begin", "event_declaration_end", "stream_declaration",
  "stream_declaration_begin", "stream_declaration_end", "env_declaration",
  "env_declaration_begin", "env_declaration_end", "trace_declaration",
  "trace_declaration_begin", "trace_declaration_end", "clock_declaration",
  "clock_declaration_begin", "clock_declaration_end",
  "callsite_declaration", "callsite_declaration_begin",
  "callsite_declaration_end", "integer_declaration_specifiers",
  "declaration_specifiers", "type_declarator_list",
  "integer_type_specifier", "type_specifier", "struct_type_specifier",
  "struct_declaration_begin", "struct_declaration_end",
  "variant_type_specifier", "variant_declaration_begin",
  "variant_declaration_end", "enum_type_specifier",
  "struct_or_variant_declaration_list", "struct_or_variant_declaration",
  "alias_declaration_specifiers", "struct_or_variant_declarator_list",
  "struct_or_variant_declarator", "enumerator_list", "enumerator",
  "abstract_declarator_list", "abstract_declarator",
  "direct_abstract_declarator", "alias_abstract_declarator_list",
  "alias_abstract_declarator", "direct_alias_abstract_declarator",
  "declarator", "direct_declarator", "type_declarator",
  "direct_type_declarator", "pointer", "type_qualifier_list",
  "ctf_assignment_expression_list", "ctf_assignment_expression", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310
};
# endif

#define YYPACT_NINF -359

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-359)))

#define YYTABLE_NINF -33

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    1967,  -359,  -359,  -359,    41,    56,    63,    80,  -359,   100,
    -359,  -359,  -359,  -359,   114,   122,    44,   154,   159,   185,
    2057,  2057,  -359,    65,  -359,  -359,  -359,  -359,  -359,   396,
    -359,  -359,   448,  -359,   500,  -359,   552,  -359,   604,  -359,
    -359,  1937,  -359,  1678,  2114,   167,   170,  -359,  -359,  -359,
     656,   708,  -359,   760,  -359,   193,   193,  -359,  -359,  -359,
    -359,   812,  -359,   864,  1755,  1785,  -359,   -13,   124,   172,
    -359,  -359,  -359,  -359,  -359,  -359,  -359,  1280,    38,  1332,
    1332,   115,   136,   156,    41,  -359,  -359,    80,   224,   100,
     239,   253,   271,   297,  -359,    93,    44,  -359,  -359,  -359,
    2057,  2057,   330,    65,   392,   442,   449,   501,  -359,  -359,
     553,  -359,   119,   138,  -359,  1997,   448,   186,   192,  -359,
     500,   199,  -359,   552,   214,  -359,   604,  -359,  -359,  1815,
    -359,   216,  -359,  -359,  -359,  -359,  -359,  -359,  -359,  -359,
    -359,  -359,  -359,  -359,  -359,  -359,  -359,  -359,  -359,  -359,
    -359,  -359,   218,   221,   225,    28,  -359,  -359,  -359,   245,
    -359,  -359,  -359,  -359,  -359,  -359,  -359,   189,  -359,  1678,
    2114,  1678,  2114,  -359,   916,  -359,   968,  -359,  1020,  -359,
    -359,  1875,   240,  -359,   812,   248,  -359,   864,    11,    24,
    -359,   173,  -359,   268,     9,    25,  -359,    45,  -359,   289,
      10,   273,   290,   131,  -359,   252,  -359,  1905,  -359,   305,
    -359,   119,   119,  1755,  1785,  1280,   274,   287,  2057,  1280,
    1815,  -359,   302,  -359,  -359,  -359,  -359,  -359,  -359,  -359,
    1785,    69,  1280,  1280,  1280,  1280,  -359,  1383,  1072,  1678,
    -359,  -359,    48,   335,    50,   357,  -359,  -359,  -359,  1875,
    1875,  -359,  2057,  2057,  1725,   265,  -359,  -359,  -359,  -359,
    -359,   320,  -359,  -359,   126,  2027,    11,  1176,   268,   326,
    -359,    25,  1280,   289,   346,   346,   341,   345,  1905,   347,
     348,  1905,  -359,  -359,  -359,   176,   342,   367,  -359,  -359,
    -359,  -359,  2087,  -359,  1785,   342,  -359,   163,  -359,   354,
    -359,  -359,  -359,  -359,  -359,  -359,  -359,  1124,    72,  -359,
    1434,  1678,  -359,  1485,  1678,   324,   325,  1755,  1785,    35,
    1280,  1815,  -359,   197,  -359,   359,   373,    17,   372,  -359,
    -359,  -359,  -359,  -359,  -359,  1845,  -359,  -359,   378,  -359,
    -359,   388,  -359,  -359,   346,   346,  -359,   346,   346,  -359,
    2027,  -359,   342,  -359,  1280,  -359,  -359,  1536,  -359,    96,
    -359,   139,   389,   391,   184,   243,   393,  -359,  1785,   264,
    -359,    23,  1280,  1280,   373,  1280,   196,  -359,  -359,  -359,
     267,  -359,   394,   397,  -359,  -359,  1905,  1905,  -359,  -359,
    -359,  -359,  1845,  -359,  -359,  -359,  1587,  -359,  1638,  1280,
    1280,  2027,  -359,  -359,   276,  -359,  -359,  -359,   400,   399,
     403,  -359,   196,  1228,   394,  -359,  -359,  1905,  1905,  1905,
    1905,   377,  -359,  -359,   404,   407,  1845,  -359,  -359,  -359,
    -359,  -359,  -359,   402,  -359,  -359,  -359,  -359,  -359,  -359,
     284,  -359,  -359
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    87,   104,   109,     0,     0,     0,     0,   108,     0,
     106,   107,   105,   110,     0,   120,     0,     0,     0,     0,
       0,     0,   111,     0,   103,   112,   113,   114,   115,     0,
       2,    49,     0,    50,     0,    51,     0,    52,     0,    53,
      54,     0,    88,     0,     0,   161,   164,   123,    69,    61,
       0,     0,    65,     0,   134,   129,   130,   125,   165,    73,
      81,     0,    77,     0,   197,     0,   149,     0,     0,     0,
     124,   165,     1,     3,    34,    35,    36,     0,     0,     0,
       0,    87,   104,   109,    19,    26,    24,    16,   108,    17,
     106,   107,   105,   110,    25,   120,    21,    27,    29,    28,
       0,    23,   111,    20,   103,   112,   113,   114,    30,    31,
     115,    33,    43,     0,    59,     0,     0,     0,     0,    63,
       0,     0,    67,     0,     0,    71,     0,    48,    89,     0,
      90,   188,    22,     5,    10,    19,    16,     9,    17,     7,
       8,     6,    11,    18,    21,    23,    12,    20,     4,    13,
      14,    15,   185,   186,   187,     0,   183,    83,    93,     0,
      95,    96,    94,    97,    98,    99,   100,     0,    84,     0,
       0,     0,     0,   116,     0,   118,     0,   121,     0,   165,
     165,     0,     0,    79,     0,     0,    75,     0,   197,   220,
     198,     0,   193,   195,   197,     0,   217,     0,    91,   215,
       0,     0,     0,     0,   165,     0,   165,     0,    32,     0,
      62,    44,    45,   197,     0,     0,     0,     0,     0,     0,
       0,    60,     0,   225,    66,    64,    70,    68,    74,    72,
       0,     0,     0,     0,     0,     0,   151,     0,     0,     0,
      85,    86,     0,     0,     0,     0,   117,   119,   122,     0,
       0,   135,     0,     0,     0,   126,   166,    82,    80,    78,
      76,     0,   223,   221,     0,     0,   197,     0,   196,     0,
      56,     0,     0,   216,     0,     0,     0,     0,     0,     0,
       0,     0,   150,   136,    37,     0,   230,     0,    41,    42,
      39,    40,   228,   227,     0,   231,   226,     0,    57,    47,
     192,   189,   190,   191,   157,   184,   101,     0,     0,   153,
       0,     0,   155,     0,     0,   127,   128,   197,     0,     0,
       0,     0,   212,     0,   178,   180,   210,     0,     0,   199,
     224,   222,   172,   174,   173,   206,   194,   201,     0,   218,
      92,     0,   165,   165,   141,   143,   139,   146,   148,   144,
       0,    38,   229,    55,     0,   102,   152,     0,   159,     0,
     162,     0,     0,     0,     0,     0,     0,   181,     0,     0,
     167,     0,     0,     0,   211,     0,   206,   175,   177,   176,
       0,   202,   204,   206,   200,   219,     0,     0,   165,   165,
     165,   165,   206,    46,   158,   154,     0,   156,     0,     0,
       0,     0,   169,   213,     0,   170,   179,   182,     0,     0,
       0,    58,   206,     0,   205,   137,   138,     0,     0,     0,
       0,   232,   160,   163,     0,     0,   206,   168,   214,   131,
     207,   203,   209,     0,   140,   142,   145,   147,   132,   133,
       0,   208,   171
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -359,  -359,   -41,   270,   -62,    91,   420,  -359,  -359,   350,
    -359,  -359,   337,  -359,  -359,   383,  -359,  -359,   332,  -359,
    -359,   281,  -359,  -359,   285,    61,     0,  -128,  -158,   -38,
    -359,   288,    97,  -359,   -46,  -274,  -359,   -57,  -359,  -334,
    -359,    99,  -159,  -231,  -208,  -175,   315,  -358,  -341,   127,
     198,   191,  -187,   312,   -53,  -359,    -6,   -70
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    29,   111,   112,   113,   300,    30,    31,    32,   114,
      33,    34,   119,    35,    36,   122,    37,    38,   125,    39,
      63,   186,    40,    61,   183,   167,   115,   197,   168,    42,
      57,    58,   255,    70,    71,   283,    47,   181,   256,   335,
     323,   324,   155,   156,   191,   192,   193,   380,   381,   382,
     325,   326,   198,   199,   200,   264,   116,   117
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      41,   231,   154,   130,   346,   285,   305,   349,   269,   241,
     242,   194,   244,   261,   207,   209,   392,   188,   195,   188,
      64,    65,   204,   206,   189,   319,   130,   130,   120,    41,
     123,   319,   126,   195,   421,   410,   189,   189,   189,   236,
     201,   202,   320,   319,   174,   176,   222,   178,   189,   262,
     222,    43,   237,   222,    54,   184,   222,   187,   210,   309,
      44,   312,   190,   196,   190,   270,    48,   426,   440,   271,
     322,   431,   310,    49,   313,    66,   322,   130,   196,   305,
     308,    67,   305,   356,   340,   241,   286,   241,   322,   298,
      50,   336,   295,   271,    45,    46,   357,    55,    56,   -18,
     213,   214,   297,    53,   222,   -18,   222,   395,   222,   364,
      51,   -18,   415,   416,   222,   -18,   -18,   222,    68,    69,
     396,   -22,   249,   250,    52,   215,   305,   -22,   154,   230,
     154,   216,    53,   -22,    66,   194,   263,   -22,   -22,   189,
     203,   217,    -5,   434,   435,   436,   437,   278,    -5,   281,
     397,   330,   359,   287,    -5,   361,   218,   293,    -5,    -5,
     194,   219,   -10,   398,    59,   305,   352,   305,   -10,    60,
     299,   299,   299,   299,   -10,   130,   130,   169,   -10,   -10,
     171,   254,    66,   353,   276,   277,   170,   271,   205,   172,
     365,   265,   130,   369,   350,    62,   154,   266,   154,   239,
     266,   327,   401,    54,   376,   338,   223,   254,   266,   189,
     341,   331,   224,   194,   240,   158,   130,   370,   292,   226,
     294,   371,   159,   160,   161,   162,   163,   334,   342,   343,
      -9,   243,   307,   245,   228,   164,    -9,   222,   165,   232,
     404,   233,    -9,   166,   234,    -7,    -9,    -9,   235,   254,
     254,    -7,   317,   318,   130,   238,   130,    -7,   367,    -8,
     257,    -7,    -7,   402,   194,    -8,   327,   271,   259,   154,
     154,    -8,   154,   154,   267,    -8,    -8,    -6,   254,   130,
     130,   254,   383,    -6,   405,   386,   387,   411,   271,    -6,
     274,   412,   393,    -6,    -6,   272,   427,   379,   388,   389,
     271,   390,   391,   -11,   442,   279,   280,   275,   412,   -11,
     407,   408,   334,   409,   284,   -11,   154,   328,   327,   -11,
     -11,   368,   296,   383,   301,   302,   303,   288,   289,   329,
     130,   417,   418,   419,   420,   339,   -12,   424,   425,   383,
     290,   291,   -12,   179,   180,   311,   315,   316,   -12,   211,
     212,   433,   -12,   -12,   379,   154,    66,   154,   344,   383,
     240,   158,   345,   334,   347,   348,   271,   314,   159,   160,
     161,   162,   163,   383,   351,   354,   362,   363,   372,   373,
     375,   164,   240,   158,   165,   384,   254,   254,   379,   166,
     159,   160,   161,   162,   163,   385,    72,   399,    -4,   400,
     413,   412,   403,   164,    -4,   376,   165,   428,   429,   441,
      -4,   166,   430,   438,    -4,    -4,   439,   254,   254,   254,
     254,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,   -13,    73,
      28,    74,    75,    76,   -13,   -14,    77,   225,   229,    78,
     -13,   -14,    79,    80,   -13,   -13,   221,   -14,   260,   258,
     406,   -14,   -14,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,    74,    75,    76,   227,   -15,    77,   268,
     414,   118,   273,   -15,    79,    80,     0,   366,   374,   -15,
       0,     0,     0,   -15,   -15,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,    74,    75,    76,     0,   -32,
      77,     0,     0,   121,     0,   -32,    79,    80,     0,     0,
       0,   -32,     0,     0,     0,   -32,   -32,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,    74,    75,    76,
       0,     0,    77,     0,     0,   124,     0,     0,    79,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,    74,
      75,    76,     0,     0,    77,     0,     0,   173,     0,     0,
      79,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,    74,    75,    76,     0,     0,    77,     0,     0,   175,
       0,     0,    79,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,    74,    75,    76,     0,     0,    77,     0,
       0,   177,     0,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,    74,    75,    76,     0,     0,
      77,     0,     0,   182,     0,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,    74,    75,    76,
       0,     0,    77,     0,     0,   185,     0,     0,    79,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,    74,
      75,    76,     0,     0,    77,     0,     0,   246,     0,     0,
      79,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,    74,    75,    76,     0,     0,    77,     0,     0,   247,
       0,     0,    79,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,    74,    75,    76,     0,     0,    77,     0,
       0,   248,     0,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,    74,    75,    76,     0,     0,
      77,     0,     0,   306,     0,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,    74,    75,    76,
       0,     0,    77,     0,     0,   355,     0,     0,    79,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,    74,
      75,    76,     0,   337,    77,     0,     0,     0,     0,     0,
      79,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,   133,   134,   135,    85,    86,   136,   137,   138,
     139,   140,   141,   142,    94,   143,   144,    97,    98,    99,
       0,   145,   146,   147,   148,   149,   150,   151,   108,   109,
     208,    74,    75,    76,     0,   432,    77,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   132,   133,   134,   135,    85,    86,   136,
     137,   138,   139,   140,   141,   142,    94,   143,   144,    97,
      98,    99,     0,   145,   146,   147,   148,   149,   150,   151,
     108,   109,   208,    74,    75,    76,     0,     0,    77,     0,
       0,     0,     0,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,   133,   134,   135,    85,
      86,   136,   137,   138,   139,   140,   141,   142,    94,   143,
     144,    97,    98,    99,     0,   145,   146,   147,   148,   149,
     150,   151,   108,   109,   208,    74,    75,    76,     0,     0,
      77,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   132,   133,   134,
     135,    85,    86,   136,   137,   138,   139,   140,   141,   142,
      94,   143,   144,    97,    98,    99,     0,   145,   146,   147,
     148,   149,   150,   151,   108,   109,   208,   131,     0,     0,
       0,     0,     0,     0,   304,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,   133,
     134,   135,    85,    86,   136,   137,   138,   139,   140,   141,
     142,    94,   143,   144,    97,    98,    99,     0,   145,   146,
     147,   148,   149,   150,   151,   108,   152,   153,   131,     0,
       0,     0,     0,     0,     0,   358,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   132,
     133,   134,   135,    85,    86,   136,   137,   138,   139,   140,
     141,   142,    94,   143,   144,    97,    98,    99,     0,   145,
     146,   147,   148,   149,   150,   151,   108,   152,   153,   131,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     132,   133,   134,   135,    85,    86,   136,   137,   138,   139,
     140,   141,   142,    94,   143,   144,    97,    98,    99,     0,
     145,   146,   147,   148,   149,   150,   151,   108,   152,   153,
     131,     0,     0,     0,     0,     0,     0,   394,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,   133,   134,   135,    85,    86,   136,   137,   138,
     139,   140,   141,   142,    94,   143,   144,    97,    98,    99,
       0,   145,   146,   147,   148,   149,   150,   151,   108,   152,
     153,   131,     0,     0,     0,     0,     0,     0,   422,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   132,   133,   134,   135,    85,    86,   136,   137,
     138,   139,   140,   141,   142,    94,   143,   144,    97,    98,
      99,     0,   145,   146,   147,   148,   149,   150,   151,   108,
     152,   153,   131,     0,     0,     0,     0,     0,     0,   423,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   132,   133,   134,   135,    85,    86,   136,
     137,   138,   139,   140,   141,   142,    94,   143,   144,    97,
      98,    99,   131,   145,   146,   147,   148,   149,   150,   151,
     108,   152,   153,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   132,   133,   134,   135,    85,    86,   136,
     137,   138,   139,   140,   141,   142,    94,   143,   144,    97,
      98,    99,     0,   145,   146,   147,   148,   149,   150,   151,
     108,   152,   153,   319,     0,     0,     0,     0,   189,     0,
       0,     0,     0,     0,   320,     0,     0,     0,     0,     0,
     128,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,   188,    15,    16,     0,     0,   189,     0,
     321,    22,    23,    24,    25,    26,    27,     0,   322,    28,
     128,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,   195,    15,    16,     0,     0,   189,     0,
       0,    22,    23,    24,    25,    26,    27,     0,   190,    28,
     128,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,   195,    15,    16,     0,     0,   189,     0,
       0,    22,    23,    24,    25,    26,    27,     0,   196,    28,
       1,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,   376,    15,    16,     0,     0,   189,     0,
       0,    22,    23,    24,    25,    26,    27,     0,   196,    28,
     377,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,     0,    15,    16,   251,     0,     0,     0,
       0,    22,    23,    24,    25,    26,    27,     0,   378,    28,
       1,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,     0,    15,    16,   282,     0,     0,   252,
     253,    22,    23,    24,    25,    26,    27,     0,     0,    28,
       1,     2,     3,     4,     0,     0,     7,     8,     9,    10,
      11,    12,    13,     0,    15,    16,     0,     0,     0,   252,
     253,    22,    23,    24,    25,    26,    27,   127,     0,    28,
       0,     0,   128,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,   129,    22,    23,    24,    25,    26,    27,     0,
       0,    28,     1,     2,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,     0,
       0,    28,   128,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,   220,    22,    23,    24,    25,    26,    27,     0,
       0,    28,   332,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,     0,    22,    23,    24,    25,    26,    27,     0,
     333,    28,     1,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,     0,    22,    23,    24,    25,    26,    27,     0,
       0,    28,   128,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,     0,    22,    23,    24,    25,    26,    27,   157,
     158,    28,     0,     0,     0,     0,     0,   159,   160,   161,
     162,   163,     0,     0,     0,     0,     0,     0,     0,     0,
     164,     0,     0,   165,     0,     0,     0,     0,   166
};

static const yytype_int16 yycheck[] =
{
       0,   129,    43,    41,   278,   213,   237,   281,   195,   167,
     169,    64,   171,   188,    71,    77,   350,     8,     8,     8,
      20,    21,    68,    69,    13,     8,    64,    65,    34,    29,
      36,     8,    38,     8,   392,   376,    13,    13,    13,    11,
      53,    54,    19,     8,    50,    51,   116,    53,    13,    25,
     120,    10,    24,   123,    10,    61,   126,    63,    20,    11,
      19,    11,    53,    53,    53,    20,    10,   401,   426,    24,
      53,   412,    24,    10,    24,    10,    53,   115,    53,   310,
     239,    16,   313,    11,   271,   243,   214,   245,    53,    20,
      10,   266,   220,    24,    53,    54,    24,    53,    54,     6,
     100,   101,   230,    10,   174,    12,   176,    11,   178,   317,
      10,    18,   386,   387,   184,    22,    23,   187,    53,    54,
      24,     6,   179,   180,    10,     6,   357,    12,   169,   129,
     171,    12,    10,    18,    10,   188,   189,    22,    23,    13,
      16,    22,     6,   417,   418,   419,   420,   204,    12,   206,
      11,    25,   311,   215,    18,   314,    18,   219,    22,    23,
     213,    23,     6,    24,    10,   396,   294,   398,    12,    10,
     232,   233,   234,   235,    18,   213,   214,    10,    22,    23,
      10,   181,    10,    20,    53,    54,    19,    24,    16,    19,
     318,    18,   230,   321,    18,    10,   237,    24,   239,    10,
      24,   254,    18,    10,     8,   267,    20,   207,    24,    13,
     272,   264,    20,   266,    25,    26,   254,    20,   218,    20,
     220,    24,    33,    34,    35,    36,    37,   265,   274,   275,
       6,   170,   238,   172,    20,    46,    12,   307,    49,    23,
     368,    23,    18,    54,    23,     6,    22,    23,    23,   249,
     250,    12,   252,   253,   292,    10,   294,    18,   320,     6,
      20,    22,    23,    20,   317,    12,   319,    24,    20,   310,
     311,    18,   313,   314,     6,    22,    23,     6,   278,   317,
     318,   281,   335,    12,    20,   342,   343,    20,    24,    18,
      17,    24,   354,    22,    23,     6,    20,   335,   344,   345,
      24,   347,   348,     6,    20,    53,    54,    17,    24,    12,
     372,   373,   350,   375,     9,    18,   357,    52,   371,    22,
      23,   321,    20,   376,   233,   234,   235,    53,    54,     9,
     368,   388,   389,   390,   391,     9,     6,   399,   400,   392,
      53,    54,    12,    55,    56,    10,   249,   250,    18,    79,
      80,   413,    22,    23,   392,   396,    10,   398,    17,   412,
      25,    26,    17,   401,    17,    17,    24,    10,    33,    34,
      35,    36,    37,   426,     7,    21,    52,    52,    19,     6,
       8,    46,    25,    26,    49,     7,   386,   387,   426,    54,
      33,    34,    35,    36,    37,     7,     0,     8,     6,     8,
       6,    24,     9,    46,    12,     8,    49,     7,     9,     7,
      18,    54,     9,     9,    22,    23,     9,   417,   418,   419,
     420,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     6,    29,
      54,     3,     4,     5,    12,     6,     8,   120,   126,    11,
      18,    12,    14,    15,    22,    23,   116,    18,   187,   184,
     371,    22,    23,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     3,     4,     5,   123,     6,     8,   194,
     383,    11,   200,    12,    14,    15,    -1,   319,   327,    18,
      -1,    -1,    -1,    22,    23,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     3,     4,     5,    -1,     6,
       8,    -1,    -1,    11,    -1,    12,    14,    15,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    22,    23,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     3,     4,     5,
      -1,    -1,     8,    -1,    -1,    11,    -1,    -1,    14,    15,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     3,
       4,     5,    -1,    -1,     8,    -1,    -1,    11,    -1,    -1,
      14,    15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     3,     4,     5,    -1,    -1,     8,    -1,    -1,    11,
      -1,    -1,    14,    15,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     3,     4,     5,    -1,    -1,     8,    -1,
      -1,    11,    -1,    -1,    14,    15,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     3,     4,     5,    -1,    -1,
       8,    -1,    -1,    11,    -1,    -1,    14,    15,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     3,     4,     5,
      -1,    -1,     8,    -1,    -1,    11,    -1,    -1,    14,    15,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     3,
       4,     5,    -1,    -1,     8,    -1,    -1,    11,    -1,    -1,
      14,    15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     3,     4,     5,    -1,    -1,     8,    -1,    -1,    11,
      -1,    -1,    14,    15,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     3,     4,     5,    -1,    -1,     8,    -1,
      -1,    11,    -1,    -1,    14,    15,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     3,     4,     5,    -1,    -1,
       8,    -1,    -1,    11,    -1,    -1,    14,    15,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     3,     4,     5,
      -1,    -1,     8,    -1,    -1,    11,    -1,    -1,    14,    15,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     3,
       4,     5,    -1,     7,     8,    -1,    -1,    -1,    -1,    -1,
      14,    15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     3,     4,     5,    -1,     7,     8,    -1,    -1,    -1,
      -1,    -1,    14,    15,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     3,     4,     5,    -1,    -1,     8,    -1,
      -1,    -1,    -1,    -1,    14,    15,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     3,     4,     5,    -1,    -1,
       8,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     4,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     4,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,     4,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     8,    -1,    -1,    -1,    -1,    13,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,    -1,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,     8,    39,    40,    -1,    -1,    13,    -1,
      45,    46,    47,    48,    49,    50,    51,    -1,    53,    54,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,     8,    39,    40,    -1,    -1,    13,    -1,
      -1,    46,    47,    48,    49,    50,    51,    -1,    53,    54,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,     8,    39,    40,    -1,    -1,    13,    -1,
      -1,    46,    47,    48,    49,    50,    51,    -1,    53,    54,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,     8,    39,    40,    -1,    -1,    13,    -1,
      -1,    46,    47,    48,    49,    50,    51,    -1,    53,    54,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,    -1,    39,    40,    11,    -1,    -1,    -1,
      -1,    46,    47,    48,    49,    50,    51,    -1,    53,    54,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,    -1,    39,    40,    11,    -1,    -1,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,    54,
      25,    26,    27,    28,    -1,    -1,    31,    32,    33,    34,
      35,    36,    37,    -1,    39,    40,    -1,    -1,    -1,    44,
      45,    46,    47,    48,    49,    50,    51,    20,    -1,    54,
      -1,    -1,    25,    26,    27,    28,    -1,    -1,    31,    32,
      33,    34,    35,    36,    37,    -1,    39,    40,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    54,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    54,    25,    26,    27,    28,    -1,    -1,    31,    32,
      33,    34,    35,    36,    37,    -1,    39,    40,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    54,    25,    26,    27,    28,    -1,    -1,    31,    32,
      33,    34,    35,    36,    37,    -1,    39,    40,    -1,    -1,
      -1,    -1,    -1,    46,    47,    48,    49,    50,    51,    -1,
      53,    54,    25,    26,    27,    28,    -1,    -1,    31,    32,
      33,    34,    35,    36,    37,    -1,    39,    40,    -1,    -1,
      -1,    -1,    -1,    46,    47,    48,    49,    50,    51,    -1,
      -1,    54,    25,    26,    27,    28,    -1,    -1,    31,    32,
      33,    34,    35,    36,    37,    -1,    39,    40,    -1,    -1,
      -1,    -1,    -1,    46,    47,    48,    49,    50,    51,    25,
      26,    54,    -1,    -1,    -1,    -1,    -1,    33,    34,    35,
      36,    37,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    -1,    -1,    -1,    -1,    54
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    54,    57,
      62,    63,    64,    66,    67,    69,    70,    72,    73,    75,
      78,    82,    85,    10,    19,    53,    54,    92,    10,    10,
      10,    10,    10,    10,    10,    53,    54,    86,    87,    10,
      10,    79,    10,    76,    82,    82,    10,    16,    53,    54,
      89,    90,     0,    62,     3,     4,     5,     8,    11,    14,
      15,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    58,    59,    60,    65,    82,   112,   113,    11,    68,
     112,    11,    71,   112,    11,    74,   112,    20,    25,    45,
      85,     4,    25,    26,    27,    28,    31,    32,    33,    34,
      35,    36,    37,    39,    40,    45,    46,    47,    48,    49,
      50,    51,    53,    54,    58,    98,    99,    25,    26,    33,
      34,    35,    36,    37,    46,    49,    54,    81,    84,    10,
      19,    10,    19,    11,   112,    11,   112,    11,   112,    87,
      87,    93,    11,    80,   112,    11,    77,   112,     8,    13,
      53,   100,   101,   102,   110,     8,    53,    83,   108,   109,
     110,    53,    54,    16,    90,    16,    90,    93,    54,    60,
      20,    59,    59,    82,    82,     6,    12,    22,    18,    23,
      45,    65,   113,    20,    20,    68,    20,    71,    20,    74,
      82,    83,    23,    23,    23,    23,    11,    24,    10,    10,
      25,    84,    98,    81,    98,    81,    11,    11,    11,    93,
      93,    11,    44,    45,    82,    88,    94,    20,    80,    20,
      77,   101,    25,   110,   111,    18,    24,     6,   102,   108,
      20,    24,     6,   109,    17,    17,    53,    54,    93,    53,
      54,    93,    11,    91,     9,   100,    83,    60,    53,    54,
      53,    54,    82,    60,    82,    83,    20,    83,    20,    60,
      61,    61,    61,    61,    11,    99,    11,   112,    98,    11,
      24,    10,    11,    24,    10,    88,    88,    82,    82,     8,
      19,    45,    53,    96,    97,   106,   107,   110,    52,     9,
      25,   110,    25,    53,    85,    95,   101,     7,    60,     9,
     108,    60,    90,    90,    17,    17,    91,    17,    17,    91,
      18,     7,    83,    20,    21,    11,    11,    24,    11,    98,
      11,    98,    52,    52,   100,    83,   106,    60,    82,    83,
      20,    24,    19,     6,   107,     8,     8,    25,    53,    85,
     103,   104,   105,   110,     7,     7,    93,    93,    90,    90,
      90,    90,    95,    60,    11,    11,    24,    11,    24,     8,
       8,    18,    20,     9,    83,    20,    97,    60,    60,    60,
     104,    20,    24,     6,   105,    91,    91,    93,    93,    93,
      93,   103,    11,    11,    60,    60,    95,    20,     7,     9,
       9,   104,     7,    60,    91,    91,    91,    91,     9,     9,
     103,     7,    20
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    56,    57,    57,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    59,    59,    59,    59,    59,    59,    59,    59,    59,
      59,    59,    59,    60,    60,    60,    61,    61,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    63,
      63,    64,    65,    66,    66,    67,    68,    69,    69,    70,
      71,    72,    72,    73,    74,    75,    75,    76,    77,    78,
      78,    79,    80,    81,    81,    81,    81,    82,    82,    82,
      82,    83,    83,    84,    84,    84,    84,    84,    84,    84,
      84,    84,    84,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    86,    86,    86,    86,
      86,    86,    86,    86,    87,    88,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    90,
      91,    92,    92,    92,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    92,    93,    93,    94,    94,    94,
      94,    94,    95,    95,    95,    95,    95,    95,    96,    96,
      97,    97,    97,    98,    98,    99,    99,    99,    99,    99,
      99,    99,    99,   100,   100,   101,   101,   102,   102,   102,
     102,   102,   103,   103,   104,   104,   105,   105,   105,   105,
     106,   106,   107,   107,   107,   108,   108,   109,   109,   109,
     110,   110,   110,   111,   111,   112,   112,   113,   113,   113,
     113,   113,   113
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     3,
       3,     3,     3,     1,     2,     2,     3,     1,     2,     1,
       1,     1,     1,     1,     1,     5,     4,     4,     7,     2,
       3,     2,     2,     2,     3,     2,     2,     2,     3,     2,
       2,     2,     3,     2,     2,     3,     4,     1,     2,     3,
       4,     1,     2,     1,     1,     2,     2,     1,     1,     2,
       2,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     3,     4,
       1,     3,     4,     2,     2,     2,     3,     4,     4,     1,
       1,     7,     8,     8,     1,     1,     3,     6,     6,     4,
       7,     4,     7,     4,     4,     7,     4,     7,     4,     1,
       1,     3,     5,     4,     6,     4,     6,     4,     6,     5,
       7,     1,     5,     7,     1,     0,     2,     3,     5,     4,
       4,     7,     1,     1,     1,     2,     2,     2,     1,     3,
       1,     2,     3,     1,     3,     1,     1,     1,     1,     3,
       3,     3,     3,     1,     3,     1,     2,     0,     1,     3,
       4,     3,     1,     3,     1,     2,     0,     3,     4,     3,
       1,     2,     1,     3,     4,     1,     2,     1,     3,     4,
       1,     2,     3,     1,     2,     2,     3,     3,     3,     4,
       3,     3,     6
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (scanner, yyscanner, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, scanner, yyscanner); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, struct ctf_scanner *scanner, yyscan_t yyscanner)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (scanner);
  YYUSE (yyscanner);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, struct ctf_scanner *scanner, yyscan_t yyscanner)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, scanner, yyscanner);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, struct ctf_scanner *scanner, yyscan_t yyscanner)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              , scanner, yyscanner);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, scanner, yyscanner); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, struct ctf_scanner *scanner, yyscan_t yyscanner)
{
  YYUSE (yyvaluep);
  YYUSE (scanner);
  YYUSE (yyscanner);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (struct ctf_scanner *scanner, yyscan_t yyscanner)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, yyscanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1116 "ctf-parser.y" /* yacc.c:1646  */
    {
			if (set_parent_node((yyvsp[0].n), &ctf_scanner_get_ast(scanner)->root))
				reparent_error(scanner, "error reparenting to root");
		}
#line 3037 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1121 "ctf-parser.y" /* yacc.c:1646  */
    {
			if (set_parent_node((yyvsp[0].n), &ctf_scanner_get_ast(scanner)->root))
				reparent_error(scanner, "error reparenting to root");
		}
#line 3046 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1129 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3052 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1131 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3058 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1133 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3064 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1135 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3070 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1137 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3076 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 9:
#line 1139 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3082 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1141 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3088 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1143 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3094 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1145 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3100 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1147 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3106 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1149 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3112 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1151 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3118 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1153 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3124 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1155 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3130 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1157 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3136 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1159 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3142 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1161 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3148 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1163 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3154 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1165 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3160 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1167 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3166 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1169 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3172 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1171 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3178 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1173 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3184 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1175 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3190 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1177 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3196 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1179 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3202 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1181 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.s) = yylval.s;		}
#line 3208 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1189 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
		}
#line 3218 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1195 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
		}
#line 3228 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1201 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
		}
#line 3238 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1207 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			(yyval.n)->u.unary_expression.u.unsigned_constant = (yyvsp[0].ull);
		}
#line 3248 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1213 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = (yyvsp[0].s);
		}
#line 3258 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1219 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = (yyvsp[0].s);
		}
#line 3268 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1225 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-1].n);
		}
#line 3276 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1229 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_SBRAC;
			(yyval.n)->u.unary_expression.u.sbrac_exp = (yyvsp[-1].n);
			bt_list_splice(&((yyvsp[-3].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
#line 3288 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1237 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
			(yyval.n)->u.unary_expression.link = UNARY_DOTLINK;
			bt_list_splice(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
#line 3301 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1246 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
			(yyval.n)->u.unary_expression.link = UNARY_DOTLINK;
			bt_list_splice(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
#line 3314 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1255 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
			(yyval.n)->u.unary_expression.link = UNARY_ARROWLINK;
			bt_list_splice(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
#line 3327 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1264 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.s;
			(yyval.n)->u.unary_expression.link = UNARY_ARROWLINK;
			bt_list_splice(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
#line 3340 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1276 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);				}
#line 3346 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1278 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[0].n);
			if ((yyval.n)->u.unary_expression.type != UNARY_UNSIGNED_CONSTANT
				&& (yyval.n)->u.unary_expression.type != UNARY_SIGNED_CONSTANT) {
				reparent_error(scanner, "expecting numeric constant");
			}
		}
#line 3358 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1286 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[0].n);
			if ((yyval.n)->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
				(yyval.n)->u.unary_expression.type = UNARY_SIGNED_CONSTANT;
				(yyval.n)->u.unary_expression.u.signed_constant =
					-((yyval.n)->u.unary_expression.u.unsigned_constant);
			} else if ((yyval.n)->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
				(yyval.n)->u.unary_expression.u.signed_constant =
					-((yyval.n)->u.unary_expression.u.signed_constant);
			} else {
				reparent_error(scanner, "expecting numeric constant");
			}
		}
#line 3376 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1303 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			_bt_list_splice_tail(&((yyvsp[0].n))->tmp_head, &((yyval.n))->tmp_head);
			(yyvsp[0].n)->u.unary_expression.link = UNARY_DOTDOTDOT;
		}
#line 3386 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1309 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);		}
#line 3392 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1316 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[-1].n);	}
#line 3398 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 49:
#line 1318 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3404 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1320 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3410 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 1322 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3416 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 52:
#line 1324 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3422 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1326 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3428 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1328 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3434 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1330 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-4].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 3449 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1341 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 3463 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1351 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-3].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 3477 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1361 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEALIAS);
			(yyval.n)->u.typealias.target = make_node(scanner, NODE_TYPEALIAS_TARGET);
			(yyval.n)->u.typealias.alias = make_node(scanner, NODE_TYPEALIAS_ALIAS);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.target->u.typealias_target.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-5].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-4].n))->tmp_head, &((yyval.n))->u.typealias.target->u.typealias_target.type_declarators);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.alias->u.typealias_alias.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.typealias.alias->u.typealias_alias.type_declarators);
		}
#line 3499 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1382 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_EVENT);
		}
#line 3507 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1386 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_EVENT);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "event_declaration");
		}
#line 3517 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1395 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 3523 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1400 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 3529 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1406 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STREAM);
		}
#line 3537 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1410 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STREAM);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "stream_declaration");
		}
#line 3547 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1419 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 3553 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1424 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 3559 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1429 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENV);
		}
#line 3567 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1433 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENV);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "env declaration");
		}
#line 3577 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1442 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 3583 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 70:
#line 1447 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 3589 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 71:
#line 1452 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TRACE);
		}
#line 3597 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1456 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TRACE);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
#line 3607 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1465 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 3613 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1470 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 3619 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1475 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_CLOCK);
		}
#line 3627 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1479 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_CLOCK);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
#line 3637 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1488 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 3643 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1493 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 3649 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1498 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_CALLSITE);
		}
#line 3657 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1502 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_CALLSITE);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
#line 3667 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1511 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 3673 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 82:
#line 1516 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 3679 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1521 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3692 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1530 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[0].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3704 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1538 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[-1].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3717 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1547 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-1].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3726 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1555 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3739 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1564 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[0].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3751 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1572 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[-1].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3764 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1581 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-1].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 3773 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1589 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 3779 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1591 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->tmp_head);
		}
#line 3788 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1599 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_CHAR;
		}
#line 3797 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1604 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SHORT;
		}
#line 3806 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1609 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INT;
		}
#line 3815 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1614 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_LONG;
		}
#line 3824 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1619 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SIGNED;
		}
#line 3833 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1624 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_UNSIGNED;
		}
#line 3842 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1629 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_BOOL;
		}
#line 3851 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1634 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ID_TYPE;
			(yyval.n)->u.type_specifier.id_type = yylval.s;
		}
#line 3861 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1640 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
		}
#line 3871 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1646 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "integer reparent error");
		}
#line 3883 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1657 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_VOID;
		}
#line 3892 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1662 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_CHAR;
		}
#line 3901 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 105:
#line 1667 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SHORT;
		}
#line 3910 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1672 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INT;
		}
#line 3919 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1677 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_LONG;
		}
#line 3928 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1682 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOAT;
		}
#line 3937 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 109:
#line 1687 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_DOUBLE;
		}
#line 3946 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 110:
#line 1692 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SIGNED;
		}
#line 3955 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 111:
#line 1697 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_UNSIGNED;
		}
#line 3964 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1702 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_BOOL;
		}
#line 3973 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 113:
#line 1707 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_COMPLEX;
		}
#line 3982 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 114:
#line 1712 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_IMAGINARY;
		}
#line 3991 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 115:
#line 1717 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ID_TYPE;
			(yyval.n)->u.type_specifier.id_type = yylval.s;
		}
#line 4001 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 116:
#line 1723 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_FLOATING_POINT);
		}
#line 4011 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1729 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_FLOATING_POINT);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "floating point reparent error");
		}
#line 4023 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1737 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
		}
#line 4033 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 119:
#line 1743 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "integer reparent error");
		}
#line 4045 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1751 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
		}
#line 4055 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1757 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
		}
#line 4065 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 122:
#line 1763 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
			if (set_parent_node((yyvsp[-1].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "string reparent error");
		}
#line 4077 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 123:
#line 1771 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ENUM;
			(yyval.n)->u.type_specifier.node = (yyvsp[0].n);
		}
#line 4087 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1777 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_VARIANT;
			(yyval.n)->u.type_specifier.node = (yyvsp[0].n);
		}
#line 4097 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 125:
#line 1783 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRUCT;
			(yyval.n)->u.type_specifier.node = (yyvsp[0].n);
		}
#line 4107 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 126:
#line 1792 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
#line 4118 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1799 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[-3].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
#line 4130 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1807 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[-3].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
#line 4142 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1815 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 0;
			(yyval.n)->u._struct.name = (yyvsp[0].s);
		}
#line 4152 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1821 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 0;
			(yyval.n)->u._struct.name = (yyvsp[0].s);
		}
#line 4162 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1827 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			bt_list_add_tail(&((yyvsp[-1].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[-5].n) && set_parent_node((yyvsp[-5].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
#line 4174 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1835 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[-7].s);
			bt_list_add_tail(&((yyvsp[-1].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[-5].n) && set_parent_node((yyvsp[-5].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
#line 4187 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 133:
#line 1844 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[-7].s);
			bt_list_add_tail(&((yyvsp[-1].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[-5].n) && set_parent_node((yyvsp[-5].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
#line 4200 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1856 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 4206 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1861 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 4212 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 136:
#line 1866 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4223 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 137:
#line 1873 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.choice = (yyvsp[-4].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4235 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 138:
#line 1881 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.choice = (yyvsp[-4].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4247 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 139:
#line 1889 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[-3].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4259 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1897 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[-6].s);
			(yyval.n)->u.variant.choice = (yyvsp[-4].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4272 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1906 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[-3].s);
			(yyval.n)->u.variant.choice = (yyvsp[-1].s);
		}
#line 4283 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 142:
#line 1913 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[-6].s);
			(yyval.n)->u.variant.choice = (yyvsp[-4].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4296 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1922 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[-3].s);
			(yyval.n)->u.variant.choice = (yyvsp[-1].s);
		}
#line 4307 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 144:
#line 1929 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[-3].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4319 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1937 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[-6].s);
			(yyval.n)->u.variant.choice = (yyvsp[-4].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4332 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 146:
#line 1946 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[-3].s);
			(yyval.n)->u.variant.choice = (yyvsp[-1].s);
		}
#line 4343 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1953 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[-6].s);
			(yyval.n)->u.variant.choice = (yyvsp[-4].s);
			if ((yyvsp[-1].n) && set_parent_node((yyvsp[-1].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
#line 4356 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 148:
#line 1962 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[-3].s);
			(yyval.n)->u.variant.choice = (yyvsp[-1].s);
		}
#line 4367 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 149:
#line 1972 "ctf-parser.y" /* yacc.c:1646  */
    {	push_scope(scanner);	}
#line 4373 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 150:
#line 1977 "ctf-parser.y" /* yacc.c:1646  */
    {	pop_scope(scanner);	}
#line 4379 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 151:
#line 1982 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4389 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 152:
#line 1988 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			((yyval.n))->u._enum.container_type = (yyvsp[-3].n);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4400 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 153:
#line 1995 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-3].s);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4411 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 154:
#line 2002 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-5].s);
			((yyval.n))->u._enum.container_type = (yyvsp[-3].n);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4423 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 155:
#line 2010 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-3].s);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4434 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 156:
#line 2017 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-5].s);
			((yyval.n))->u._enum.container_type = (yyvsp[-3].n);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4446 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 157:
#line 2025 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4456 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 158:
#line 2031 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			((yyval.n))->u._enum.container_type = (yyvsp[-4].n);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4467 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 159:
#line 2038 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-4].s);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4478 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 160:
#line 2045 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-6].s);
			((yyval.n))->u._enum.container_type = (yyvsp[-4].n);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4490 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 161:
#line 2053 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 0;
			(yyval.n)->u._enum.enum_id = (yyvsp[0].s);
		}
#line 4500 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 162:
#line 2059 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-4].s);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4511 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 163:
#line 2066 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[-6].s);
			((yyval.n))->u._enum.container_type = (yyvsp[-4].n);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
#line 4523 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 164:
#line 2074 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 0;
			(yyval.n)->u._enum.enum_id = (yyvsp[0].s);
		}
#line 4533 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 165:
#line 2083 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = NULL;	}
#line 4539 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 166:
#line 2085 "ctf-parser.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-1].n)) {
				(yyval.n) = (yyvsp[-1].n);
				bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->tmp_head);
			} else {
				(yyval.n) = (yyvsp[0].n);
				bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
			}
		}
#line 4553 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 167:
#line 2098 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_STRUCT_OR_VARIANT_DECLARATION);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.struct_or_variant_declaration.type_declarators);
		}
#line 4567 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 168:
#line 2108 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-4].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 4582 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 169:
#line 2119 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 4596 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 170:
#line 2129 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[-3].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 4610 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 171:
#line 2139 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEALIAS);
			(yyval.n)->u.typealias.target = make_node(scanner, NODE_TYPEALIAS_TARGET);
			(yyval.n)->u.typealias.alias = make_node(scanner, NODE_TYPEALIAS_ALIAS);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.target->u.typealias_target.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-5].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-4].n))->tmp_head, &((yyval.n))->u.typealias.target->u.typealias_target.type_declarators);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.alias->u.typealias_alias.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.typealias.alias->u.typealias_alias.type_declarators);
		}
#line 4632 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 172:
#line 2160 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 4645 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 173:
#line 2169 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[0].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 4657 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 174:
#line 2177 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			add_type(scanner, (yyvsp[0].s));
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_ID_TYPE;
			node->u.type_specifier.id_type = yylval.s;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 4672 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 175:
#line 2188 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[-1].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 4685 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 176:
#line 2197 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-1].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 4694 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 177:
#line 2202 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *node;

			add_type(scanner, (yyvsp[0].s));
			(yyval.n) = (yyvsp[-1].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_ID_TYPE;
			node->u.type_specifier.id_type = yylval.s;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
#line 4709 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 178:
#line 2216 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4715 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 179:
#line 2218 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->tmp_head);
		}
#line 4724 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 180:
#line 2226 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4730 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 181:
#line 2228 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4736 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 182:
#line 2230 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			if (set_parent_node((yyvsp[0].n), (yyvsp[-2].n)))
				reparent_error(scanner, "struct_or_variant_declarator");
		}
#line 4746 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 183:
#line 2239 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4752 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 184:
#line 2241 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->tmp_head);
		}
#line 4761 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 185:
#line 2249 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[0].s);
		}
#line 4770 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 186:
#line 2254 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[0].s);
		}
#line 4779 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 187:
#line 2259 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[0].s);
		}
#line 4788 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 188:
#line 2264 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[0].s);
		}
#line 4797 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 189:
#line 2269 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[-2].s);
			bt_list_splice(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
#line 4807 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 190:
#line 2275 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[-2].s);
			bt_list_splice(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
#line 4817 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 191:
#line 2281 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[-2].s);
			bt_list_splice(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
#line 4827 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 192:
#line 2287 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[-2].s);
			bt_list_splice(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
#line 4837 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 193:
#line 2296 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4843 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 194:
#line 2298 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->tmp_head);
		}
#line 4852 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 195:
#line 2306 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4858 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 196:
#line 2308 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[0].n);
			bt_list_splice(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
#line 4867 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 197:
#line 2316 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
                        (yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			/* id is NULL */
		}
#line 4877 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 198:
#line 2322 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[0].s);
		}
#line 4887 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 199:
#line 2328 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-1].n);
		}
#line 4897 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 200:
#line 2334 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-3].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
#line 4909 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 201:
#line 2342 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-2].n);
			(yyval.n)->u.type_declarator.u.nested.abstract_array = 1;
		}
#line 4920 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 202:
#line 2352 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4926 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 203:
#line 2354 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->tmp_head);
		}
#line 4935 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 204:
#line 2362 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4941 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 205:
#line 2364 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[0].n);
			bt_list_splice(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
#line 4950 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 206:
#line 2372 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
                        (yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			/* id is NULL */
		}
#line 4960 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 207:
#line 2378 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-1].n);
		}
#line 4970 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 208:
#line 2384 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-3].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
#line 4982 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 209:
#line 2392 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-2].n);
			(yyval.n)->u.type_declarator.u.nested.abstract_array = 1;
		}
#line 4993 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 210:
#line 2402 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 4999 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 211:
#line 2404 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[0].n);
			bt_list_splice(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
#line 5008 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 212:
#line 2412 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[0].s);
		}
#line 5018 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 213:
#line 2418 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-1].n);
		}
#line 5028 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 214:
#line 2424 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-3].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
#line 5040 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 215:
#line 2435 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[0].n);	}
#line 5046 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 216:
#line 2437 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[0].n);
			bt_list_splice(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
#line 5055 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 217:
#line 2445 "ctf-parser.y" /* yacc.c:1646  */
    {
			add_type(scanner, (yyvsp[0].s));
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[0].s);
		}
#line 5066 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 218:
#line 2452 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-1].n);
		}
#line 5076 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 219:
#line 2458 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[-3].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[-1].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
#line 5088 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 220:
#line 2469 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
		}
#line 5096 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 221:
#line 2473 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
			bt_list_splice(&((yyvsp[0].n))->tmp_head, &((yyval.n))->tmp_head);
		}
#line 5105 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 222:
#line 2478 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
			(yyval.n)->u.pointer.const_qualifier = 1;
			bt_list_splice(&((yyvsp[0].n))->tmp_head, &((yyval.n))->tmp_head);
		}
#line 5115 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 225:
#line 2495 "ctf-parser.y" /* yacc.c:1646  */
    {	(yyval.n) = (yyvsp[-1].n);	}
#line 5121 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 226:
#line 2497 "ctf-parser.y" /* yacc.c:1646  */
    {
			(yyval.n) = (yyvsp[-2].n);
			bt_list_add_tail(&((yyvsp[-1].n))->siblings, &((yyval.n))->tmp_head);
		}
#line 5130 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 227:
#line 2505 "ctf-parser.y" /* yacc.c:1646  */
    {
			/*
			 * Because we have left and right, cannot use
			 * set_parent_node.
			 */
			(yyval.n) = make_node(scanner, NODE_CTF_EXPRESSION);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u.ctf_expression.left);
			if ((yyvsp[-2].n)->u.unary_expression.type != UNARY_STRING)
				reparent_error(scanner, "ctf_assignment_expression left expects string");
			_bt_list_splice_tail(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u.ctf_expression.right);
		}
#line 5146 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 228:
#line 2517 "ctf-parser.y" /* yacc.c:1646  */
    {
			/*
			 * Because we have left and right, cannot use
			 * set_parent_node.
			 */
			(yyval.n) = make_node(scanner, NODE_CTF_EXPRESSION);
			_bt_list_splice_tail(&((yyvsp[-2].n))->tmp_head, &((yyval.n))->u.ctf_expression.left);
			if ((yyvsp[-2].n)->u.unary_expression.type != UNARY_STRING)
				reparent_error(scanner, "ctf_assignment_expression left expects string");
			bt_list_add_tail(&((yyvsp[0].n))->siblings, &((yyval.n))->u.ctf_expression.right);
		}
#line 5162 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 229:
#line 2529 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[-3].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-1].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 5177 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 230:
#line 2540 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-1].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 5191 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 231:
#line 2550 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[-2].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
#line 5205 "ctf-parser.c" /* yacc.c:1646  */
    break;

  case 232:
#line 2560 "ctf-parser.y" /* yacc.c:1646  */
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEALIAS);
			(yyval.n)->u.typealias.target = make_node(scanner, NODE_TYPEALIAS_TARGET);
			(yyval.n)->u.typealias.alias = make_node(scanner, NODE_TYPEALIAS_ALIAS);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.target->u.typealias_target.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-4].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[-3].n))->tmp_head, &((yyval.n))->u.typealias.target->u.typealias_target.type_declarators);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.alias->u.typealias_alias.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[-1].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[0].n))->tmp_head, &((yyval.n))->u.typealias.alias->u.typealias_alias.type_declarators);
		}
#line 5227 "ctf-parser.c" /* yacc.c:1646  */
    break;


#line 5231 "ctf-parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (scanner, yyscanner, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (scanner, yyscanner, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, scanner, yyscanner);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, scanner, yyscanner);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (scanner, yyscanner, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, scanner, yyscanner);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, scanner, yyscanner);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
