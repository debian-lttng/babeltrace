/* A Bison parser, made by GNU Bison 2.6.2.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.6.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
/* Line 336 of yacc.c  */
#line 1 "ctf-parser.y"

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

__attribute__((visibility("hidden")))
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

__attribute__((visibility("hidden")))
int yyparse(struct ctf_scanner *scanner);
__attribute__((visibility("hidden")))
int yylex(union YYSTYPE *yyval, struct ctf_scanner *scanner);
__attribute__((visibility("hidden")))
int yylex_init_extra(struct ctf_scanner *scanner, yyscan_t * ptr_yy_globals);
__attribute__((visibility("hidden")))
int yylex_destroy(yyscan_t yyscanner);
__attribute__((visibility("hidden")))
void yyrestart(FILE * in_str, yyscan_t scanner);

struct gc_string {
	struct bt_list_head gc;
	size_t alloclen;
	char s[];
};

static const char *node_type_to_str[] = {
	[ NODE_UNKNOWN ] = "NODE_UNKNOWN",
	[ NODE_ROOT ] = "NODE_ROOT",
	[ NODE_EVENT ] = "NODE_EVENT",
	[ NODE_ENV ] = "NODE_ENV",
	[ NODE_STREAM ] = "NODE_STREAM",
	[ NODE_TRACE ] = "NODE_TRACE",
	[ NODE_CLOCK ] = "NODE_CLOCK",
	[ NODE_CALLSITE ] = "NODE_CALLSITE",
	[ NODE_CTF_EXPRESSION ] = "NODE_CTF_EXPRESSION",
	[ NODE_UNARY_EXPRESSION ] = "NODE_UNARY_EXPRESSION",
	[ NODE_TYPEDEF ] = "NODE_TYPEDEF",
	[ NODE_TYPEALIAS_TARGET ] = "NODE_TYPEALIAS_TARGET",
	[ NODE_TYPEALIAS_ALIAS ] = "NODE_TYPEALIAS_ALIAS",
	[ NODE_TYPEALIAS ] = "NODE_TYPEALIAS",
	[ NODE_TYPE_SPECIFIER ] = "NODE_TYPE_SPECIFIER",
	[ NODE_TYPE_SPECIFIER_LIST ] = "NODE_TYPE_SPECIFIER_LIST",
	[ NODE_POINTER ] = "NODE_POINTER",
	[ NODE_TYPE_DECLARATOR ] = "NODE_TYPE_DECLARATOR",
	[ NODE_FLOATING_POINT ] = "NODE_FLOATING_POINT",
	[ NODE_INTEGER ] = "NODE_INTEGER",
	[ NODE_STRING ] = "NODE_STRING",
	[ NODE_ENUMERATOR ] = "NODE_ENUMERATOR",
	[ NODE_ENUM ] = "NODE_ENUM",
	[ NODE_STRUCT_OR_VARIANT_DECLARATION ] = "NODE_STRUCT_OR_VARIANT_DECLARATION",
	[ NODE_VARIANT ] = "NODE_VARIANT",
	[ NODE_STRUCT ] = "NODE_STRUCT",
};

__attribute__((visibility("hidden")))
const char *node_type(struct ctf_node *node)
{
	if (node->type < NR_NODE_TYPES)
		return node_type_to_str[node->type];
	else
		return NULL;
}

static struct gc_string *gc_string_alloc(struct ctf_scanner *scanner,
					 size_t len)
{
	struct gc_string *gstr;
	size_t alloclen;

	/* TODO: could be faster with find first bit or glib Gstring */
	/* sizeof long to account for malloc header (int or long ?) */
	for (alloclen = 8; alloclen < sizeof(long) + sizeof(*gstr) + len;
	     alloclen *= 2);

	gstr = malloc(alloclen);
	bt_list_add(&gstr->gc, &scanner->allocated_strings);
	gstr->alloclen = alloclen;
	return gstr;
}

/*
 * note: never use gc_string_append on a string that has external references.
 * gsrc will be garbage collected immediately, and gstr might be.
 * Should only be used to append characters to a string literal or constant.
 */
__attribute__((visibility("hidden")))
struct gc_string *gc_string_append(struct ctf_scanner *scanner,
				   struct gc_string *gstr,
				   struct gc_string *gsrc)
{
	size_t newlen = strlen(gsrc->s) + strlen(gstr->s) + 1;
	size_t alloclen;

	/* TODO: could be faster with find first bit or glib Gstring */
	/* sizeof long to account for malloc header (int or long ?) */
	for (alloclen = 8; alloclen < sizeof(long) + sizeof(*gstr) + newlen;
	     alloclen *= 2);

	if (alloclen > gstr->alloclen) {
		struct gc_string *newgstr;

		newgstr = gc_string_alloc(scanner, newlen);
		strcpy(newgstr->s, gstr->s);
		strcat(newgstr->s, gsrc->s);
		bt_list_del(&gstr->gc);
		free(gstr);
		gstr = newgstr;
	} else {
		strcat(gstr->s, gsrc->s);
	}
	bt_list_del(&gsrc->gc);
	free(gsrc);
	return gstr;
}

void setstring(struct ctf_scanner *scanner, YYSTYPE *lvalp, const char *src)
{
	lvalp->gs = gc_string_alloc(scanner, strlen(src) + 1);
	strcpy(lvalp->gs->s, src);
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

__attribute__((visibility("hidden")))
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

static void add_type(struct ctf_scanner *scanner, struct gc_string *id)
{
	printf_debug("add type %s\n", id->s);
	if (lookup_type(scanner->cs, id->s))
		return;
	g_hash_table_insert(scanner->cs->types, id->s, id->s);
}

static struct ctf_node *make_node(struct ctf_scanner *scanner,
				  enum node_type type)
{
	struct ctf_ast *ast = ctf_scanner_get_ast(scanner);
	struct ctf_node *node;

	node = malloc(sizeof(*node));
	if (!node)
		return NULL;
	memset(node, 0, sizeof(*node));
	node->type = type;
	BT_INIT_LIST_HEAD(&node->tmp_head);
	bt_list_add(&node->gc, &ast->allocated_nodes);
	bt_list_add(&node->siblings, &node->tmp_head);

	switch (type) {
	case NODE_ROOT:
		fprintf(stderr, "[error] %s: trying to create root node\n", __func__);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) type);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
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
		fprintf(stderr, "[error] %s: trying to reparent root node\n", __func__);
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
		fprintf(stderr, "[error] %s: unknown node type %d\n", __func__,
			(int) parent->type);
		return -EINVAL;
	}
	return 0;
}

__attribute__((visibility("hidden")))
void yyerror(struct ctf_scanner *scanner, const char *str)
{
	fprintf(stderr, "error %s\n", str);
}
 
__attribute__((visibility("hidden")))
int yywrap(void)
{
	return 1;
} 

#define reparent_error(scanner, str)				\
do {								\
	yyerror(scanner, YY_("reparent_error: " str "\n"));	\
	YYERROR;						\
} while (0)

static void free_strings(struct bt_list_head *list)
{
	struct gc_string *gstr, *tmp;

	bt_list_for_each_entry_safe(gstr, tmp, list, gc)
		free(gstr);
}

static struct ctf_ast *ctf_ast_alloc(void)
{
	struct ctf_ast *ast;

	ast = malloc(sizeof(*ast));
	if (!ast)
		return NULL;
	memset(ast, 0, sizeof(*ast));
	BT_INIT_LIST_HEAD(&ast->allocated_nodes);
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

static void ctf_ast_free(struct ctf_ast *ast)
{
	struct ctf_node *node, *tmp;

	bt_list_for_each_entry_safe(node, tmp, &ast->allocated_nodes, gc)
		free(node);
	free(ast);
}

int ctf_scanner_append_ast(struct ctf_scanner *scanner)
{
	return yyparse(scanner);
}

struct ctf_scanner *ctf_scanner_alloc(FILE *input)
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
		fprintf(stderr, "yylex_init error\n");
		goto cleanup_scanner;
	}
	/* Start processing new stream */
	yyrestart(input, scanner->scanner);

	scanner->ast = ctf_ast_alloc();
	if (!scanner->ast)
		goto cleanup_lexer;
	init_scope(&scanner->root_scope, NULL);
	scanner->cs = &scanner->root_scope;
	BT_INIT_LIST_HEAD(&scanner->allocated_strings);

	if (yydebug)
		fprintf(stdout, "Scanner input is a%s.\n",
			isatty(fileno(input)) ? "n interactive tty" :
						" noninteractive file");

	return scanner;

cleanup_lexer:
	ret = yylex_destroy(scanner->scanner);
	if (!ret)
		fprintf(stderr, "yylex_destroy error\n");
cleanup_scanner:
	free(scanner);
	return NULL;
}

void ctf_scanner_free(struct ctf_scanner *scanner)
{
	int ret;

	finalize_scope(&scanner->root_scope);
	free_strings(&scanner->allocated_strings);
	ctf_ast_free(scanner->ast);
	ret = yylex_destroy(scanner->scanner);
	if (ret)
		fprintf(stderr, "yylex_destroy error\n");
	free(scanner);
}


/* Line 336 of yacc.c  */
#line 1004 "ctf-parser.c"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_Y_TAB_H
# define YY_Y_TAB_H
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     CHARACTER_CONSTANT_START = 258,
     SQUOTE = 259,
     STRING_LITERAL_START = 260,
     DQUOTE = 261,
     ESCSEQ = 262,
     CHAR_STRING_TOKEN = 263,
     LSBRAC = 264,
     RSBRAC = 265,
     LPAREN = 266,
     RPAREN = 267,
     LBRAC = 268,
     RBRAC = 269,
     RARROW = 270,
     STAR = 271,
     PLUS = 272,
     MINUS = 273,
     LT = 274,
     GT = 275,
     TYPEASSIGN = 276,
     COLON = 277,
     SEMICOLON = 278,
     DOTDOTDOT = 279,
     DOT = 280,
     EQUAL = 281,
     COMMA = 282,
     CONST = 283,
     CHAR = 284,
     DOUBLE = 285,
     ENUM = 286,
     ENV = 287,
     EVENT = 288,
     FLOATING_POINT = 289,
     FLOAT = 290,
     INTEGER = 291,
     INT = 292,
     LONG = 293,
     SHORT = 294,
     SIGNED = 295,
     STREAM = 296,
     STRING = 297,
     STRUCT = 298,
     TRACE = 299,
     CALLSITE = 300,
     CLOCK = 301,
     TYPEALIAS = 302,
     TYPEDEF = 303,
     UNSIGNED = 304,
     VARIANT = 305,
     VOID = 306,
     _BOOL = 307,
     _COMPLEX = 308,
     _IMAGINARY = 309,
     DECIMAL_CONSTANT = 310,
     OCTAL_CONSTANT = 311,
     HEXADECIMAL_CONSTANT = 312,
     TOK_ALIGN = 313,
     IDENTIFIER = 314,
     ID_TYPE = 315,
     ERROR = 316
   };
#endif
/* Tokens.  */
#define CHARACTER_CONSTANT_START 258
#define SQUOTE 259
#define STRING_LITERAL_START 260
#define DQUOTE 261
#define ESCSEQ 262
#define CHAR_STRING_TOKEN 263
#define LSBRAC 264
#define RSBRAC 265
#define LPAREN 266
#define RPAREN 267
#define LBRAC 268
#define RBRAC 269
#define RARROW 270
#define STAR 271
#define PLUS 272
#define MINUS 273
#define LT 274
#define GT 275
#define TYPEASSIGN 276
#define COLON 277
#define SEMICOLON 278
#define DOTDOTDOT 279
#define DOT 280
#define EQUAL 281
#define COMMA 282
#define CONST 283
#define CHAR 284
#define DOUBLE 285
#define ENUM 286
#define ENV 287
#define EVENT 288
#define FLOATING_POINT 289
#define FLOAT 290
#define INTEGER 291
#define INT 292
#define LONG 293
#define SHORT 294
#define SIGNED 295
#define STREAM 296
#define STRING 297
#define STRUCT 298
#define TRACE 299
#define CALLSITE 300
#define CLOCK 301
#define TYPEALIAS 302
#define TYPEDEF 303
#define UNSIGNED 304
#define VARIANT 305
#define VOID 306
#define _BOOL 307
#define _COMPLEX 308
#define _IMAGINARY 309
#define DECIMAL_CONSTANT 310
#define OCTAL_CONSTANT 311
#define HEXADECIMAL_CONSTANT 312
#define TOK_ALIGN 313
#define IDENTIFIER 314
#define ID_TYPE 315
#define ERROR 316



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 350 of yacc.c  */
#line 955 "ctf-parser.y"

	long long ll;
	char c;
	struct gc_string *gs;
	struct ctf_node *n;


/* Line 350 of yacc.c  */
#line 1177 "ctf-parser.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (struct ctf_scanner *scanner);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */

#endif /* !YY_Y_TAB_H  */

/* Copy the second part of user declarations.  */

/* Line 353 of yacc.c  */
#line 1204 "ctf-parser.c"

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
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
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
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

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
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  72
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2321

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  62
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  62
/* YYNRULES -- Number of rules.  */
#define YYNRULES  245
/* YYNRULES -- Number of states.  */
#define YYNSTATES  463

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
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
      55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    14,    16,    18,
      20,    22,    24,    26,    28,    30,    32,    34,    36,    38,
      40,    42,    44,    46,    48,    50,    52,    54,    56,    58,
      60,    62,    64,    67,    69,    71,    73,    76,    78,    80,
      82,    84,    86,    88,    90,    92,    95,    99,   103,   107,
     112,   116,   120,   124,   128,   130,   133,   136,   140,   142,
     145,   147,   149,   151,   153,   155,   157,   163,   168,   173,
     181,   184,   188,   191,   194,   197,   201,   204,   207,   210,
     214,   217,   220,   223,   227,   230,   233,   237,   242,   244,
     247,   251,   256,   258,   261,   263,   265,   268,   271,   273,
     275,   278,   281,   283,   287,   289,   291,   293,   295,   297,
     299,   301,   303,   307,   312,   314,   316,   318,   320,   322,
     324,   326,   328,   330,   332,   334,   336,   338,   342,   347,
     351,   356,   358,   362,   367,   370,   373,   376,   380,   385,
     390,   392,   394,   402,   411,   420,   422,   424,   428,   435,
     442,   447,   455,   460,   468,   473,   478,   486,   491,   499,
     504,   506,   508,   512,   518,   523,   530,   535,   542,   547,
     554,   560,   568,   570,   576,   584,   586,   587,   590,   594,
     600,   605,   610,   618,   620,   622,   624,   627,   630,   633,
     635,   639,   641,   644,   648,   650,   654,   656,   658,   660,
     663,   667,   671,   675,   679,   684,   690,   692,   696,   698,
     701,   702,   704,   708,   713,   717,   719,   723,   725,   728,
     729,   733,   738,   742,   744,   747,   749,   753,   758,   760,
     763,   765,   769,   774,   776,   779,   783,   785,   788,   791,
     795,   799,   803,   808,   812,   816
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      63,     0,    -1,    72,    -1,    63,    72,    -1,    51,    -1,
      29,    -1,    39,    -1,    37,    -1,    38,    -1,    35,    -1,
      30,    -1,    40,    -1,    49,    -1,    52,    -1,    53,    -1,
      54,    -1,    34,    -1,    36,    -1,    42,    -1,    31,    -1,
      50,    -1,    43,    -1,    28,    -1,    48,    -1,    33,    -1,
      41,    -1,    32,    -1,    44,    -1,    46,    -1,    45,    -1,
      58,    -1,    66,    -1,    65,    66,    -1,     8,    -1,     7,
      -1,    68,    -1,    67,    68,    -1,     8,    -1,     7,    -1,
      59,    -1,    60,    -1,    64,    -1,    55,    -1,    56,    -1,
      57,    -1,     5,     6,    -1,     5,    67,     6,    -1,     3,
      65,     4,    -1,    11,    70,    12,    -1,    69,     9,    70,
      10,    -1,    69,    25,    59,    -1,    69,    25,    60,    -1,
      69,    15,    59,    -1,    69,    15,    60,    -1,    69,    -1,
      17,    69,    -1,    18,    69,    -1,    70,    24,    70,    -1,
      70,    -1,    92,    23,    -1,    73,    -1,    76,    -1,    79,
      -1,    82,    -1,    85,    -1,    88,    -1,    92,    48,    92,
      93,    23,    -1,    48,    92,    93,    23,    -1,    92,    48,
      93,    23,    -1,    47,    92,   110,    21,   105,   113,    23,
      -1,    74,    75,    -1,    74,   122,    75,    -1,    33,    13,
      -1,    14,    23,    -1,    77,    78,    -1,    77,   122,    78,
      -1,    41,    13,    -1,    14,    23,    -1,    80,    81,    -1,
      80,   122,    81,    -1,    32,    13,    -1,    14,    23,    -1,
      83,    84,    -1,    83,   122,    84,    -1,    44,    13,    -1,
      14,    23,    -1,    46,    86,    87,    -1,    46,    86,   122,
      87,    -1,    13,    -1,    14,    23,    -1,    45,    89,    90,
      -1,    45,    89,   122,    90,    -1,    13,    -1,    14,    23,
      -1,    28,    -1,    94,    -1,    91,    28,    -1,    91,    94,
      -1,    28,    -1,    95,    -1,    92,    28,    -1,    92,    95,
      -1,   118,    -1,    93,    27,   118,    -1,    29,    -1,    39,
      -1,    37,    -1,    38,    -1,    40,    -1,    49,    -1,    52,
      -1,    60,    -1,    36,    13,    14,    -1,    36,    13,   122,
      14,    -1,    51,    -1,    29,    -1,    39,    -1,    37,    -1,
      38,    -1,    35,    -1,    30,    -1,    40,    -1,    49,    -1,
      52,    -1,    53,    -1,    54,    -1,    60,    -1,    34,    13,
      14,    -1,    34,    13,   122,    14,    -1,    36,    13,    14,
      -1,    36,    13,   122,    14,    -1,    42,    -1,    42,    13,
      14,    -1,    42,    13,   122,    14,    -1,    31,   102,    -1,
      50,    99,    -1,    43,    96,    -1,    97,   103,    98,    -1,
      59,    97,   103,    98,    -1,    60,    97,   103,    98,    -1,
      59,    -1,    60,    -1,    97,   103,    98,    58,    11,    70,
      12,    -1,    59,    97,   103,    98,    58,    11,    70,    12,
      -1,    60,    97,   103,    98,    58,    11,    70,    12,    -1,
      13,    -1,    14,    -1,   100,   103,   101,    -1,    19,    59,
      20,   100,   103,   101,    -1,    19,    60,    20,   100,   103,
     101,    -1,    59,   100,   103,   101,    -1,    59,    19,    59,
      20,   100,   103,   101,    -1,    59,    19,    59,    20,    -1,
      59,    19,    60,    20,   100,   103,   101,    -1,    59,    19,
      60,    20,    -1,    60,   100,   103,   101,    -1,    60,    19,
      59,    20,   100,   103,   101,    -1,    60,    19,    59,    20,
      -1,    60,    19,    60,    20,   100,   103,   101,    -1,    60,
      19,    60,    20,    -1,    13,    -1,    14,    -1,    13,   108,
      14,    -1,    22,    91,    13,   108,    14,    -1,    59,    13,
     108,    14,    -1,    59,    22,    91,    13,   108,    14,    -1,
      60,    13,   108,    14,    -1,    60,    22,    91,    13,   108,
      14,    -1,    13,   108,    27,    14,    -1,    22,    91,    13,
     108,    27,    14,    -1,    59,    13,   108,    27,    14,    -1,
      59,    22,    91,    13,   108,    27,    14,    -1,    59,    -1,
      60,    13,   108,    27,    14,    -1,    60,    22,    91,    13,
     108,    27,    14,    -1,    60,    -1,    -1,   103,   104,    -1,
      92,   106,    23,    -1,    92,    48,    92,    93,    23,    -1,
      48,    92,    93,    23,    -1,    92,    48,    93,    23,    -1,
      47,    92,   110,    21,   105,   113,    23,    -1,    28,    -1,
      95,    -1,    59,    -1,   105,    28,    -1,   105,    95,    -1,
     105,    59,    -1,   107,    -1,   106,    27,   107,    -1,   116,
      -1,    22,    70,    -1,   116,    22,    70,    -1,   109,    -1,
     108,    27,   109,    -1,    59,    -1,    60,    -1,    64,    -1,
       5,     6,    -1,     5,    67,     6,    -1,    59,    26,    71,
      -1,    60,    26,    71,    -1,    64,    26,    71,    -1,     5,
       6,    26,    71,    -1,     5,    67,     6,    26,    71,    -1,
     111,    -1,   110,    27,   111,    -1,   112,    -1,   120,   112,
      -1,    -1,    59,    -1,    11,   111,    12,    -1,   112,     9,
      70,    10,    -1,   112,     9,    10,    -1,   114,    -1,   113,
      27,   114,    -1,   115,    -1,   120,   115,    -1,    -1,    11,
     114,    12,    -1,   115,     9,    70,    10,    -1,   115,     9,
      10,    -1,   117,    -1,   120,   117,    -1,    59,    -1,    11,
     116,    12,    -1,   117,     9,    70,    10,    -1,   119,    -1,
     120,   119,    -1,    59,    -1,    11,   118,    12,    -1,   119,
       9,    70,    10,    -1,    16,    -1,    16,   120,    -1,    16,
     121,   120,    -1,    28,    -1,   121,    28,    -1,   123,    23,
      -1,   122,   123,    23,    -1,    70,    26,    70,    -1,    70,
      21,    92,    -1,    92,    48,    92,    93,    -1,    48,    92,
      93,    -1,    92,    48,    93,    -1,    47,    92,   110,    21,
     105,   113,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1007,  1007,  1012,  1020,  1022,  1024,  1026,  1028,  1030,
    1032,  1034,  1036,  1038,  1040,  1042,  1044,  1046,  1048,  1050,
    1052,  1054,  1056,  1058,  1060,  1062,  1064,  1066,  1068,  1070,
    1072,  1079,  1081,  1086,  1088,  1097,  1099,  1104,  1106,  1115,
    1121,  1127,  1133,  1140,  1147,  1154,  1160,  1166,  1172,  1178,
    1186,  1195,  1204,  1213,  1225,  1227,  1229,  1248,  1254,  1261,
    1263,  1265,  1267,  1269,  1271,  1273,  1275,  1286,  1296,  1306,
    1327,  1331,  1340,  1345,  1351,  1355,  1364,  1369,  1374,  1378,
    1387,  1392,  1397,  1401,  1410,  1415,  1420,  1424,  1433,  1438,
    1443,  1447,  1456,  1461,  1466,  1475,  1483,  1492,  1500,  1509,
    1517,  1526,  1534,  1536,  1544,  1549,  1554,  1559,  1564,  1569,
    1574,  1579,  1585,  1591,  1602,  1607,  1612,  1617,  1622,  1627,
    1632,  1637,  1642,  1647,  1652,  1657,  1662,  1668,  1674,  1682,
    1688,  1696,  1702,  1708,  1716,  1722,  1728,  1737,  1744,  1752,
    1760,  1766,  1772,  1780,  1789,  1801,  1806,  1811,  1818,  1826,
    1834,  1842,  1851,  1858,  1867,  1874,  1882,  1891,  1898,  1907,
    1917,  1922,  1927,  1933,  1940,  1947,  1955,  1962,  1970,  1976,
    1983,  1990,  1998,  2004,  2011,  2019,  2029,  2030,  2043,  2053,
    2064,  2074,  2084,  2105,  2114,  2122,  2133,  2142,  2147,  2161,
    2163,  2171,  2173,  2175,  2184,  2186,  2194,  2199,  2204,  2209,
    2214,  2219,  2225,  2231,  2237,  2243,  2252,  2254,  2262,  2264,
    2273,  2278,  2284,  2290,  2298,  2308,  2310,  2318,  2320,  2329,
    2334,  2340,  2348,  2358,  2360,  2368,  2374,  2380,  2391,  2393,
    2401,  2408,  2414,  2425,  2429,  2434,  2444,  2445,  2451,  2453,
    2461,  2473,  2485,  2496,  2506,  2516
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "CHARACTER_CONSTANT_START", "SQUOTE",
  "STRING_LITERAL_START", "DQUOTE", "ESCSEQ", "CHAR_STRING_TOKEN",
  "LSBRAC", "RSBRAC", "LPAREN", "RPAREN", "LBRAC", "RBRAC", "RARROW",
  "STAR", "PLUS", "MINUS", "LT", "GT", "TYPEASSIGN", "COLON", "SEMICOLON",
  "DOTDOTDOT", "DOT", "EQUAL", "COMMA", "CONST", "CHAR", "DOUBLE", "ENUM",
  "ENV", "EVENT", "FLOATING_POINT", "FLOAT", "INTEGER", "INT", "LONG",
  "SHORT", "SIGNED", "STREAM", "STRING", "STRUCT", "TRACE", "CALLSITE",
  "CLOCK", "TYPEALIAS", "TYPEDEF", "UNSIGNED", "VARIANT", "VOID", "_BOOL",
  "_COMPLEX", "_IMAGINARY", "DECIMAL_CONSTANT", "OCTAL_CONSTANT",
  "HEXADECIMAL_CONSTANT", "TOK_ALIGN", "IDENTIFIER", "ID_TYPE", "ERROR",
  "$accept", "file", "keywords", "c_char_sequence", "c_char",
  "s_char_sequence", "s_char", "postfix_expression", "unary_expression",
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
  "ctf_assignment_expression_list", "ctf_assignment_expression", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    62,    63,    63,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    65,    65,    66,    66,    67,    67,    68,    68,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    70,    70,    70,    71,    71,    72,
      72,    72,    72,    72,    72,    72,    72,    72,    72,    72,
      73,    73,    74,    75,    76,    76,    77,    78,    79,    79,
      80,    81,    82,    82,    83,    84,    85,    85,    86,    87,
      88,    88,    89,    90,    91,    91,    91,    91,    92,    92,
      92,    92,    93,    93,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    96,    96,    96,
      96,    96,    96,    96,    96,    97,    98,    99,    99,    99,
      99,    99,    99,    99,    99,    99,    99,    99,    99,    99,
     100,   101,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   103,   103,   104,   104,
     104,   104,   104,   105,   105,   105,   105,   105,   105,   106,
     106,   107,   107,   107,   108,   108,   109,   109,   109,   109,
     109,   109,   109,   109,   109,   109,   110,   110,   111,   111,
     112,   112,   112,   112,   112,   113,   113,   114,   114,   115,
     115,   115,   115,   116,   116,   117,   117,   117,   118,   118,
     119,   119,   119,   120,   120,   120,   121,   121,   122,   122,
     123,   123,   123,   123,   123,   123
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     3,     3,     3,     4,
       3,     3,     3,     3,     1,     2,     2,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     5,     4,     4,     7,
       2,     3,     2,     2,     2,     3,     2,     2,     2,     3,
       2,     2,     2,     3,     2,     2,     3,     4,     1,     2,
       3,     4,     1,     2,     1,     1,     2,     2,     1,     1,
       2,     2,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     4,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     3,
       4,     1,     3,     4,     2,     2,     2,     3,     4,     4,
       1,     1,     7,     8,     8,     1,     1,     3,     6,     6,
       4,     7,     4,     7,     4,     4,     7,     4,     7,     4,
       1,     1,     3,     5,     4,     6,     4,     6,     4,     6,
       5,     7,     1,     5,     7,     1,     0,     2,     3,     5,
       4,     4,     7,     1,     1,     1,     2,     2,     2,     1,
       3,     1,     2,     3,     1,     3,     1,     1,     1,     2,
       3,     3,     3,     3,     4,     5,     1,     3,     1,     2,
       0,     1,     3,     4,     3,     1,     3,     1,     2,     0,
       3,     4,     3,     1,     2,     1,     3,     4,     1,     2,
       1,     3,     4,     1,     2,     3,     1,     2,     2,     3,
       3,     3,     4,     3,     3,     6
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    98,   115,   120,     0,     0,     0,     0,   119,     0,
     117,   118,   116,   121,     0,   131,     0,     0,     0,     0,
       0,     0,   122,     0,   114,   123,   124,   125,   126,     0,
       2,    60,     0,    61,     0,    62,     0,    63,     0,    64,
      65,     0,    99,     0,     0,   172,   175,   134,    80,    72,
       0,     0,    76,     0,   145,   140,   141,   136,   176,    84,
      92,     0,    88,     0,   210,     0,   160,     0,     0,     0,
     135,   176,     1,     3,     0,     0,     0,     0,     0,     0,
      98,   115,   120,    19,    26,    24,    16,   119,    17,   117,
     118,   116,   121,    25,   131,    21,    27,    29,    28,     0,
      23,   122,    20,   114,   123,   124,   125,    42,    43,    44,
      30,    39,   126,    41,    54,     0,    70,     0,     0,     0,
       0,    74,     0,     0,    78,     0,     0,    82,     0,    59,
     100,     0,   101,     0,    22,     5,    10,    19,    16,     9,
      17,     7,     8,     6,    11,    18,    21,    23,    12,    20,
       4,    13,    14,    15,   196,   197,   198,     0,   194,    94,
     104,     0,   106,   107,   105,   108,   109,   110,   111,     0,
      95,     0,     0,     0,     0,   127,     0,   129,     0,   132,
       0,   176,   176,     0,     0,    90,     0,     0,    86,     0,
     210,   233,   211,     0,   206,   208,   210,     0,   230,     0,
     102,   228,     0,     0,     0,     0,   176,     0,   176,     0,
      34,    33,     0,    31,    45,    38,    37,     0,    35,    40,
       0,    73,    55,    56,   210,     0,     0,     0,     0,     0,
       0,     0,    71,     0,   238,    77,    75,    81,    79,    85,
      83,     0,     0,   199,     0,     0,     0,     0,   162,     0,
       0,     0,    96,    97,     0,     0,     0,     0,   128,   130,
     133,     0,     0,   146,     0,     0,     0,   137,   177,    93,
      91,    89,    87,     0,   236,   234,     0,     0,   210,     0,
     209,     0,    67,     0,     0,   229,     0,     0,     0,     0,
       0,     0,     0,     0,   161,   147,    47,    32,    46,    36,
      48,     0,   243,     0,    52,    53,    50,    51,   241,   240,
       0,   244,   239,     0,    68,     0,   200,    58,   201,   202,
     203,   168,   195,   112,     0,     0,   164,     0,     0,   166,
       0,     0,   138,   139,   210,     0,     0,     0,     0,   225,
       0,   189,   191,   223,     0,     0,   212,   237,   235,   183,
     185,   184,   219,   207,   214,     0,   231,   103,     0,   176,
     176,   152,   154,   150,   157,   159,   155,     0,    49,   242,
      66,   204,     0,     0,   113,   163,     0,   170,     0,   173,
       0,     0,     0,     0,     0,     0,   192,     0,     0,   178,
       0,     0,     0,   224,     0,   219,   186,   188,   187,     0,
     215,   217,   219,   213,   232,     0,     0,   176,   176,   176,
     176,   219,   205,    57,   169,   165,     0,   167,     0,     0,
       0,     0,   180,   226,     0,   181,   190,   193,     0,     0,
       0,    69,   219,     0,   218,   148,   149,     0,     0,     0,
       0,   245,   171,   174,     0,     0,   219,   179,   227,   142,
     220,   216,   222,     0,   151,   153,   156,   158,   143,   144,
       0,   221,   182
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    29,   113,   212,   213,   217,   218,   114,   115,   318,
      30,    31,    32,   116,    33,    34,   121,    35,    36,   124,
      37,    38,   127,    39,    63,   188,    40,    61,   185,   169,
     117,   199,   170,    42,    57,    58,   267,    70,    71,   295,
      47,   183,   268,   352,   340,   341,   157,   158,   193,   194,
     195,   399,   400,   401,   342,   343,   200,   201,   202,   276,
     118,   119
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -368
static const yytype_int16 yypact[] =
{
    2134,  -368,  -368,  -368,    42,    36,    61,    64,  -368,    75,
    -368,  -368,  -368,  -368,    94,   136,    49,   144,   165,   188,
    2215,  2215,  -368,    52,  -368,  -368,  -368,  -368,  -368,   443,
    -368,  -368,   495,  -368,   553,  -368,   611,  -368,   669,  -368,
    -368,  2101,  -368,  1789,  2261,   123,   124,  -368,  -368,  -368,
     727,   785,  -368,   843,  -368,   200,   200,  -368,  -368,  -368,
    -368,   901,  -368,   959,  1879,  1924,  -368,    80,    32,   102,
    -368,  -368,  -368,  -368,   158,   272,  1423,   161,  1481,  1481,
     199,   228,   231,    42,  -368,  -368,    64,   251,    75,   305,
     318,   409,   427,  -368,   197,    49,  -368,  -368,  -368,  2215,
    2215,   440,    52,   490,   493,   548,   551,  -368,  -368,  -368,
    -368,  -368,   606,  -368,   109,    99,  -368,  2161,   495,   193,
     207,  -368,   553,   209,  -368,   611,   212,  -368,   669,  -368,
    -368,  1969,  -368,   279,  -368,  -368,  -368,  -368,  -368,  -368,
    -368,  -368,  -368,  -368,  -368,  -368,  -368,  -368,  -368,  -368,
    -368,  -368,  -368,  -368,   222,   229,   242,    43,  -368,  -368,
    -368,   257,  -368,  -368,  -368,  -368,  -368,  -368,  -368,   122,
    -368,  1789,  2261,  1789,  2261,  -368,  1017,  -368,  1075,  -368,
    1133,  -368,  -368,  2041,   258,  -368,   901,   266,  -368,   959,
      23,    38,  -368,    96,  -368,   113,    16,    26,  -368,    29,
    -368,   265,    17,   274,   287,   182,  -368,   185,  -368,  2068,
    -368,  -368,   172,  -368,  -368,  -368,  -368,   296,  -368,  -368,
     280,  -368,   109,   109,  1879,  1924,  1423,   191,   263,  2215,
    1423,  1969,  -368,   276,  -368,  -368,  -368,  -368,  -368,  -368,
    -368,  1924,   145,   284,   311,  1423,  1423,  1423,  -368,  1537,
    1191,  1789,  -368,  -368,    67,   329,    69,   362,  -368,  -368,
    -368,  2041,  2041,  -368,  2215,  2215,  1834,   282,  -368,  -368,
    -368,  -368,  -368,   303,  -368,  -368,    82,  2188,    23,  1307,
     113,   320,  -368,    26,  1423,   265,   324,   324,   321,   325,
    2068,   327,   331,  2068,  -368,  -368,  -368,  -368,  -368,  -368,
    -368,   117,   322,   349,  -368,  -368,  -368,  -368,  2242,  -368,
    1924,   322,  -368,   154,  -368,  1423,   334,   337,  -368,  -368,
    -368,  -368,  -368,  -368,  1249,    78,  -368,  1579,  1789,  -368,
    1621,  1789,   304,   306,  1879,  1924,    30,  1423,  1969,  -368,
     168,  -368,   341,   361,    27,   365,  -368,  -368,  -368,  -368,
    -368,  -368,  2014,  -368,  -368,   363,  -368,  -368,   367,  -368,
    -368,   324,   324,  -368,   324,   324,  -368,  2188,  -368,   322,
    -368,  -368,  1423,  1423,  -368,  -368,  1663,  -368,    89,  -368,
      92,   368,   369,   126,   169,   371,  -368,  1924,   170,  -368,
       8,  1423,  1423,   361,  1423,   153,  -368,  -368,  -368,   171,
    -368,   375,   376,  -368,  -368,  2068,  2068,  -368,  -368,  -368,
    -368,  2014,  -368,  -368,  -368,  -368,  1705,  -368,  1747,  1423,
    1423,  2188,  -368,  -368,   176,  -368,  -368,  -368,   382,   381,
     383,  -368,   153,  1365,   375,  -368,  -368,  2068,  2068,  2068,
    2068,   370,  -368,  -368,   384,   391,  2014,  -368,  -368,  -368,
    -368,  -368,  -368,   394,  -368,  -368,  -368,  -368,  -368,  -368,
     177,  -368,  -368
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -368,  -368,   -30,  -368,   196,   261,  -164,   180,    37,  -224,
     386,  -368,  -368,   294,  -368,  -368,   291,  -368,  -368,   292,
    -368,  -368,   288,  -368,  -368,   230,  -368,  -368,   234,    54,
       0,   -99,  -162,   -39,  -368,   269,    74,  -368,   -53,  -250,
    -368,   -54,  -368,  -353,  -368,    31,  -161,  -243,  -220,  -181,
     227,  -367,  -359,    24,    91,    81,  -179,   239,   -61,  -368,
      -3,  -117
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -41
static const yytype_int16 yytable[] =
{
      41,   233,   132,   196,   301,   233,   322,   253,   233,   273,
     254,   233,   256,   156,   411,   206,   208,   209,   281,   336,
      64,    65,   319,   320,   191,   132,   132,   190,   197,    41,
     337,   122,   242,   125,   190,   128,   430,   197,   336,   191,
     363,   336,   191,   366,   441,    66,   191,   176,   178,    48,
     180,   205,   282,   299,   191,    43,   283,   248,   186,   233,
     189,   233,    54,   233,    44,    66,   274,   339,   446,   233,
     249,    67,   233,   451,    49,   192,   198,    50,   132,   460,
     299,   326,   192,   329,   322,   198,   339,   322,    51,   339,
     325,   371,   375,   253,   327,   253,   330,   353,   191,   224,
     225,    45,    46,   415,   357,   376,   417,    52,    55,    56,
     347,    68,    69,   220,   383,    66,   416,   277,   226,   418,
     229,   207,   279,   278,   227,   230,   302,   261,   262,   196,
     275,   241,   311,   322,   228,   251,   171,   173,   367,   203,
     204,   156,   313,   156,   278,   172,   174,   421,   412,    53,
     252,   160,   290,   278,   293,   435,   436,    59,   161,   162,
     163,   164,   165,   196,   395,   210,   211,   378,   314,   191,
     380,   166,   283,   322,   167,   322,   296,   370,    60,   210,
     211,   283,   168,   266,   221,   132,   132,   454,   455,   456,
     457,   389,   422,   425,   431,   390,   283,   283,   432,   447,
     462,    62,   132,   283,   432,   344,   -18,   233,   -22,   266,
      53,   369,   -18,    54,   -22,   348,   234,   196,   -18,   156,
     -22,   156,   -18,   -18,   -22,   -22,   255,   132,   257,   308,
     235,   310,   237,   359,   360,   239,   384,    -5,   351,   388,
     -10,   288,   289,    -5,   291,   292,   -10,   324,   245,    -5,
     304,   305,   -10,    -5,    -5,   246,   -10,   -10,   222,   223,
      -9,   266,   266,   303,   334,   335,    -9,   309,   247,   132,
     250,   132,    -9,   196,   284,   344,    -9,    -9,   214,   215,
     216,   269,   317,   317,   317,   243,   215,   216,   424,   271,
     266,   402,   300,   266,   286,   132,   132,   156,   156,   312,
     156,   156,   298,   215,   216,   405,   406,   287,   407,   408,
     315,   409,   410,   398,    -7,   346,   355,   316,   215,   216,
      -7,   358,   306,   307,   181,   182,    -7,    -8,   351,   344,
      -7,    -7,   356,    -8,   402,   332,   333,    66,   387,    -8,
     345,   361,   328,    -8,    -8,   362,   156,   364,   132,   283,
     402,   365,   317,   437,   438,   439,   440,   252,   160,   368,
     372,   373,   381,   391,   382,   161,   162,   163,   164,   165,
     392,   402,   398,   403,   386,   331,   394,   404,   166,   419,
     420,   167,   351,   423,   433,   402,   156,   395,   156,   168,
     252,   160,   448,   449,   244,   450,   458,   432,   161,   162,
     163,   164,   165,   459,   461,   266,   266,   398,   297,   317,
     413,   166,   232,   236,   167,    73,   240,   238,    -6,   272,
     270,   426,   168,   280,    -6,   393,   434,   385,   427,   428,
      -6,   429,     0,     0,    -6,    -6,   -11,   266,   266,   266,
     266,   285,   -11,    72,     0,     0,     0,     0,   -11,   -12,
       0,     0,   -11,   -11,     0,   -12,   444,   445,     0,     0,
       0,   -12,     0,     0,     0,   -12,   -12,     0,     0,     0,
     453,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    74,    -4,
      75,     0,   -13,    28,     0,    -4,    76,     0,   -13,    77,
       0,    -4,    78,    79,   -13,    -4,    -4,     0,   -13,   -13,
       0,     0,     0,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    74,   -14,    75,     0,
     -15,     0,     0,   -14,    76,     0,   -15,   120,     0,   -14,
      78,    79,   -15,   -14,   -14,     0,   -15,   -15,     0,     0,
       0,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    74,   -40,    75,     0,     0,     0,
       0,   -40,    76,     0,     0,   123,     0,   -40,    78,    79,
       0,   -40,   -40,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    74,     0,    75,     0,     0,     0,     0,     0,
      76,     0,     0,   126,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      74,     0,    75,     0,     0,     0,     0,     0,    76,     0,
       0,   175,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    74,     0,
      75,     0,     0,     0,     0,     0,    76,     0,     0,   177,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    74,     0,    75,     0,
       0,     0,     0,     0,    76,     0,     0,   179,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    74,     0,    75,     0,     0,     0,
       0,     0,    76,     0,     0,   184,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    74,     0,    75,     0,     0,     0,     0,     0,
      76,     0,     0,   187,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      74,     0,    75,     0,     0,     0,     0,     0,    76,     0,
       0,   258,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    74,     0,
      75,     0,     0,     0,     0,     0,    76,     0,     0,   259,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    74,     0,    75,     0,
       0,     0,     0,     0,    76,     0,     0,   260,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    74,     0,    75,     0,     0,     0,
       0,     0,    76,     0,     0,   323,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    74,     0,    75,     0,     0,     0,     0,     0,
      76,     0,     0,   374,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      74,     0,    75,     0,     0,     0,     0,   354,    76,     0,
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   134,   135,   136,   137,    84,
      85,   138,   139,   140,   141,   142,   143,   144,    93,   145,
     146,    96,    97,    98,     0,   147,   148,   149,   150,   151,
     152,   153,   107,   108,   109,   110,   111,   219,    74,     0,
      75,     0,     0,     0,     0,   452,    76,     0,     0,     0,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   134,   135,   136,   137,    84,    85,   138,
     139,   140,   141,   142,   143,   144,    93,   145,   146,    96,
      97,    98,     0,   147,   148,   149,   150,   151,   152,   153,
     107,   108,   109,   110,   111,   219,    74,     0,    75,     0,
       0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   134,   135,   136,   137,    84,    85,   138,   139,   140,
     141,   142,   143,   144,    93,   145,   146,    96,    97,    98,
       0,   147,   148,   149,   150,   151,   152,   153,   107,   108,
     109,   110,   111,   219,    74,     0,    75,     0,     0,     0,
       0,     0,    76,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   134,
     135,   136,   137,    84,    85,   138,   139,   140,   141,   142,
     143,   144,    93,   145,   146,    96,    97,    98,     0,   147,
     148,   149,   150,   151,   152,   153,   107,   108,   109,   110,
     111,   219,   133,     0,     0,     0,     0,     0,     0,     0,
       0,   321,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   134,   135,   136,   137,    84,
      85,   138,   139,   140,   141,   142,   143,   144,    93,   145,
     146,    96,    97,    98,   133,   147,   148,   149,   150,   151,
     152,   153,     0,   377,     0,   110,   154,   155,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   134,   135,   136,
     137,    84,    85,   138,   139,   140,   141,   142,   143,   144,
      93,   145,   146,    96,    97,    98,   133,   147,   148,   149,
     150,   151,   152,   153,     0,   379,     0,   110,   154,   155,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   134,
     135,   136,   137,    84,    85,   138,   139,   140,   141,   142,
     143,   144,    93,   145,   146,    96,    97,    98,   133,   147,
     148,   149,   150,   151,   152,   153,     0,   414,     0,   110,
     154,   155,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   134,   135,   136,   137,    84,    85,   138,   139,   140,
     141,   142,   143,   144,    93,   145,   146,    96,    97,    98,
     133,   147,   148,   149,   150,   151,   152,   153,     0,   442,
       0,   110,   154,   155,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   134,   135,   136,   137,    84,    85,   138,
     139,   140,   141,   142,   143,   144,    93,   145,   146,    96,
      97,    98,   133,   147,   148,   149,   150,   151,   152,   153,
       0,   443,     0,   110,   154,   155,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   134,   135,   136,   137,    84,
      85,   138,   139,   140,   141,   142,   143,   144,    93,   145,
     146,    96,    97,    98,   133,   147,   148,   149,   150,   151,
     152,   153,     0,     0,     0,   110,   154,   155,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   134,   135,   136,
     137,    84,    85,   138,   139,   140,   141,   142,   143,   144,
      93,   145,   146,    96,    97,    98,     0,   147,   148,   149,
     150,   151,   152,   153,     0,   336,     0,   110,   154,   155,
     191,     0,     0,     0,     0,     0,   337,     0,     0,     0,
       0,     0,   130,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,   338,    22,    23,    24,    25,    26,    27,     0,
     190,     0,     0,   339,    28,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   130,     2,     3,
       4,     0,     0,     7,     8,     9,    10,    11,    12,    13,
       0,    15,    16,     0,     0,     0,     0,     0,    22,    23,
      24,    25,    26,    27,     0,   197,     0,     0,   192,    28,
     191,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   130,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    15,    16,     0,     0,
       0,     0,     0,    22,    23,    24,    25,    26,    27,     0,
     197,     0,     0,   198,    28,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     2,     3,
       4,     0,     0,     7,     8,     9,    10,    11,    12,    13,
       0,    15,    16,     0,     0,     0,     0,     0,    22,    23,
      24,    25,    26,    27,     0,   395,     0,     0,   198,    28,
     191,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   396,     2,     3,     4,     0,     0,     7,     8,
       9,    10,    11,    12,    13,   263,    15,    16,     0,     0,
       0,     0,     0,    22,    23,    24,    25,    26,    27,     1,
       2,     3,     4,   397,    28,     7,     8,     9,    10,    11,
      12,    13,   294,    15,    16,     0,     0,     0,   264,   265,
      22,    23,    24,    25,    26,    27,     1,     2,     3,     4,
       0,    28,     7,     8,     9,    10,    11,    12,    13,     0,
      15,    16,     0,     0,     0,   264,   265,    22,    23,    24,
      25,    26,    27,     0,   129,     0,     0,     0,    28,   130,
       2,     3,     4,     0,     0,     7,     8,     9,    10,    11,
      12,    13,     0,    15,    16,     0,     0,     0,     0,   131,
      22,    23,    24,    25,    26,    27,     0,     0,     0,     0,
       0,    28,     1,     2,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,   130,
       2,     3,     4,     0,    28,     7,     8,     9,    10,    11,
      12,    13,     0,    15,    16,     0,     0,     0,     0,   231,
      22,    23,    24,    25,    26,    27,   349,     2,     3,     4,
       0,    28,     7,     8,     9,    10,    11,    12,    13,     0,
      15,    16,     0,     0,     0,     0,     0,    22,    23,    24,
      25,    26,    27,     1,     2,     3,     4,   350,    28,     7,
       8,     9,    10,    11,    12,    13,     0,    15,    16,     0,
       0,     0,     0,     0,    22,    23,    24,    25,    26,    27,
     130,     2,     3,     4,     0,    28,     7,     8,     9,    10,
      11,    12,    13,     0,    15,    16,     0,     0,     0,   159,
     160,    22,    23,    24,    25,    26,    27,   161,   162,   163,
     164,   165,    28,     0,     0,     0,     0,     0,     0,     0,
     166,     0,     0,   167,     0,     0,     0,     0,     0,     0,
       0,   168
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-368))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
       0,   118,    41,    64,   224,   122,   249,   169,   125,   190,
     171,   128,   173,    43,   367,    68,    69,    71,   197,    11,
      20,    21,   246,   247,    16,    64,    65,    11,    11,    29,
      22,    34,   131,    36,    11,    38,   395,    11,    11,    16,
     290,    11,    16,   293,   411,    13,    16,    50,    51,    13,
      53,    19,    23,   217,    16,    13,    27,    14,    61,   176,
      63,   178,    13,   180,    22,    13,    28,    59,   421,   186,
      27,    19,   189,   432,    13,    59,    59,    13,   117,   446,
     244,    14,    59,    14,   327,    59,    59,   330,    13,    59,
     251,   315,    14,   255,    27,   257,    27,   278,    16,    99,
     100,    59,    60,    14,   283,    27,    14,    13,    59,    60,
      28,    59,    60,    76,   334,    13,    27,    21,     9,    27,
      21,    19,     9,    27,    15,    26,   225,   181,   182,   190,
     191,   131,   231,   376,    25,    13,    13,    13,    21,    59,
      60,   171,   241,   173,    27,    22,    22,    21,   372,    13,
      28,    29,   206,    27,   208,   405,   406,    13,    36,    37,
      38,    39,    40,   224,    11,     7,     8,   328,    23,    16,
     331,    49,    27,   416,    52,   418,     4,    23,    13,     7,
       8,    27,    60,   183,    23,   224,   225,   437,   438,   439,
     440,    23,    23,    23,    23,    27,    27,    27,    27,    23,
      23,    13,   241,    27,    27,   266,     9,   324,     9,   209,
      13,   310,    15,    13,    15,   276,    23,   278,    21,   249,
      21,   251,    25,    26,    25,    26,   172,   266,   174,   229,
      23,   231,    23,   286,   287,    23,   335,     9,   277,   338,
       9,    59,    60,    15,    59,    60,    15,   250,    26,    21,
      59,    60,    21,    25,    26,    26,    25,    26,    78,    79,
       9,   261,   262,   226,   264,   265,    15,   230,    26,   308,
      13,   310,    21,   334,     9,   336,    25,    26,     6,     7,
       8,    23,   245,   246,   247,     6,     7,     8,   387,    23,
     290,   352,    12,   293,    20,   334,   335,   327,   328,    23,
     330,   331,     6,     7,     8,   359,   360,    20,   361,   362,
      26,   364,   365,   352,     9,    12,   279,     6,     7,     8,
      15,   284,    59,    60,    55,    56,    21,     9,   367,   390,
      25,    26,    12,    15,   395,   261,   262,    13,   338,    21,
      58,    20,    13,    25,    26,    20,   376,    20,   387,    27,
     411,    20,   315,   407,   408,   409,   410,    28,    29,    10,
      26,    24,    58,    22,    58,    36,    37,    38,    39,    40,
       9,   432,   411,    10,   337,    13,    11,    10,    49,    11,
      11,    52,   421,    12,     9,   446,   416,    11,   418,    60,
      28,    29,    10,    12,   133,    12,    12,    27,    36,    37,
      38,    39,    40,    12,    10,   405,   406,   446,   212,   372,
     373,    49,   118,   122,    52,    29,   128,   125,     9,   189,
     186,   390,    60,   196,    15,   344,   402,   336,   391,   392,
      21,   394,    -1,    -1,    25,    26,     9,   437,   438,   439,
     440,   202,    15,     0,    -1,    -1,    -1,    -1,    21,     9,
      -1,    -1,    25,    26,    -1,    15,   419,   420,    -1,    -1,
      -1,    21,    -1,    -1,    -1,    25,    26,    -1,    -1,    -1,
     433,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     3,     9,
       5,    -1,     9,    60,    -1,    15,    11,    -1,    15,    14,
      -1,    21,    17,    18,    21,    25,    26,    -1,    25,    26,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,     3,     9,     5,    -1,
       9,    -1,    -1,    15,    11,    -1,    15,    14,    -1,    21,
      17,    18,    21,    25,    26,    -1,    25,    26,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,     3,     9,     5,    -1,    -1,    -1,
      -1,    15,    11,    -1,    -1,    14,    -1,    21,    17,    18,
      -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,     3,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    14,    -1,    -1,    17,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
       3,    -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    14,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,     3,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    14,
      -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,     3,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    14,    -1,    -1,
      17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,     3,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    14,    -1,    -1,    17,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,     3,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    14,    -1,    -1,    17,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
       3,    -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    14,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,     3,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    14,
      -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,     3,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    14,    -1,    -1,
      17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,     3,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    14,    -1,    -1,    17,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,     3,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    14,    -1,    -1,    17,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
       3,    -1,     5,    -1,    -1,    -1,    -1,    10,    11,    -1,
      -1,    -1,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,     3,    -1,
       5,    -1,    -1,    -1,    -1,    10,    11,    -1,    -1,    -1,
      -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,     3,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,
      17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,     3,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,     5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,     5,    48,    49,    50,    51,    52,
      53,    54,    -1,    14,    -1,    58,    59,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,     5,    48,    49,    50,
      51,    52,    53,    54,    -1,    14,    -1,    58,    59,    60,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,     5,    48,
      49,    50,    51,    52,    53,    54,    -1,    14,    -1,    58,
      59,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
       5,    48,    49,    50,    51,    52,    53,    54,    -1,    14,
      -1,    58,    59,    60,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,     5,    48,    49,    50,    51,    52,    53,    54,
      -1,    14,    -1,    58,    59,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,     5,    48,    49,    50,    51,    52,
      53,    54,    -1,    -1,    -1,    58,    59,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    48,    49,    50,
      51,    52,    53,    54,    -1,    11,    -1,    58,    59,    60,
      16,    -1,    -1,    -1,    -1,    -1,    22,    -1,    -1,    -1,
      -1,    -1,    28,    29,    30,    31,    -1,    -1,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      11,    -1,    -1,    59,    60,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    -1,    -1,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    -1,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    53,    54,    -1,    11,    -1,    -1,    59,    60,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    28,    29,    30,    31,    -1,    -1,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    54,    -1,
      11,    -1,    -1,    59,    60,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    -1,    -1,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    -1,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    53,    54,    -1,    11,    -1,    -1,    59,    60,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    28,    29,    30,    31,    -1,    -1,    34,    35,
      36,    37,    38,    39,    40,    14,    42,    43,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    54,    28,
      29,    30,    31,    59,    60,    34,    35,    36,    37,    38,
      39,    40,    14,    42,    43,    -1,    -1,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    28,    29,    30,    31,
      -1,    60,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    -1,    -1,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    23,    -1,    -1,    -1,    60,    28,
      29,    30,    31,    -1,    -1,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    -1,
      -1,    60,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    28,
      29,    30,    31,    -1,    60,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    28,    29,    30,    31,
      -1,    60,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    54,    28,    29,    30,    31,    59,    60,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    54,
      28,    29,    30,    31,    -1,    60,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    -1,    -1,    -1,    28,
      29,    49,    50,    51,    52,    53,    54,    36,    37,    38,
      39,    40,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    60
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    60,    63,
      72,    73,    74,    76,    77,    79,    80,    82,    83,    85,
      88,    92,    95,    13,    22,    59,    60,   102,    13,    13,
      13,    13,    13,    13,    13,    59,    60,    96,    97,    13,
      13,    89,    13,    86,    92,    92,    13,    19,    59,    60,
      99,   100,     0,    72,     3,     5,    11,    14,    17,    18,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    64,    69,    70,    75,    92,   122,   123,
      14,    78,   122,    14,    81,   122,    14,    84,   122,    23,
      28,    48,    95,     5,    28,    29,    30,    31,    34,    35,
      36,    37,    38,    39,    40,    42,    43,    48,    49,    50,
      51,    52,    53,    54,    59,    60,    64,   108,   109,    28,
      29,    36,    37,    38,    39,    40,    49,    52,    60,    91,
      94,    13,    22,    13,    22,    14,   122,    14,   122,    14,
     122,    97,    97,   103,    14,    90,   122,    14,    87,   122,
      11,    16,    59,   110,   111,   112,   120,    11,    59,    93,
     118,   119,   120,    59,    60,    19,   100,    19,   100,   103,
       7,     8,    65,    66,     6,     7,     8,    67,    68,    60,
      70,    23,    69,    69,    92,    92,     9,    15,    25,    21,
      26,    48,    75,   123,    23,    23,    78,    23,    81,    23,
      84,    92,    93,     6,    67,    26,    26,    26,    14,    27,
      13,    13,    28,    94,   108,    91,   108,    91,    14,    14,
      14,   103,   103,    14,    47,    48,    92,    98,   104,    23,
      90,    23,    87,   111,    28,   120,   121,    21,    27,     9,
     112,   118,    23,    27,     9,   119,    20,    20,    59,    60,
     103,    59,    60,   103,    14,   101,     4,    66,     6,    68,
      12,   110,    93,    70,    59,    60,    59,    60,    92,    70,
      92,    93,    23,    93,    23,    26,     6,    70,    71,    71,
      71,    14,   109,    14,   122,   108,    14,    27,    13,    14,
      27,    13,    98,    98,    92,    92,    11,    22,    48,    59,
     106,   107,   116,   117,   120,    58,    12,    28,   120,    28,
      59,    95,   105,   111,    10,    70,    12,   118,    70,   100,
     100,    20,    20,   101,    20,    20,   101,    21,    10,    93,
      23,    71,    26,    24,    14,    14,    27,    14,   108,    14,
     108,    58,    58,   110,    93,   116,    70,    92,    93,    23,
      27,    22,     9,   117,    11,    11,    28,    59,    95,   113,
     114,   115,   120,    10,    10,   103,   103,   100,   100,   100,
     100,   105,    71,    70,    14,    14,    27,    14,    27,    11,
      11,    21,    23,    12,    93,    23,   107,    70,    70,    70,
     114,    23,    27,     9,   115,   101,   101,   103,   103,   103,
     103,   113,    14,    14,    70,    70,   105,    23,    10,    12,
      12,   114,    10,    70,   101,   101,   101,   101,    12,    12,
     113,    10,    23
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

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
      yyerror (scanner, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (YYID (N))                                                     \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (YYID (0))
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])



/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, scanner)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, scanner); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, struct ctf_scanner *scanner)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, scanner)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    struct ctf_scanner *scanner;
#endif
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
  YYUSE (scanner);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, struct ctf_scanner *scanner)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, scanner)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    struct ctf_scanner *scanner;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, scanner);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule, struct ctf_scanner *scanner)
#else
static void
yy_reduce_print (yyvsp, yyrule, scanner)
    YYSTYPE *yyvsp;
    int yyrule;
    struct ctf_scanner *scanner;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       , scanner);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule, scanner); \
} while (YYID (0))

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
#ifndef	YYINITDEPTH
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
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
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
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
                yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
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

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

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

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, struct ctf_scanner *scanner)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, scanner)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    struct ctf_scanner *scanner;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (scanner);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}




/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (struct ctf_scanner *scanner)
#else
int
yyparse (scanner)
    struct ctf_scanner *scanner;
#endif
#endif
{
/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

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
  int yytoken;
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

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;
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
      yychar = YYLEX;
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
  *++yyvsp = yylval;

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
     `$$ = $1'.

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
/* Line 1787 of yacc.c  */
#line 1008 "ctf-parser.y"
    {
			if (set_parent_node((yyvsp[(1) - (1)].n), &ctf_scanner_get_ast(scanner)->root))
				reparent_error(scanner, "error reparenting to root");
		}
    break;

  case 3:
/* Line 1787 of yacc.c  */
#line 1013 "ctf-parser.y"
    {
			if (set_parent_node((yyvsp[(2) - (2)].n), &ctf_scanner_get_ast(scanner)->root))
				reparent_error(scanner, "error reparenting to root");
		}
    break;

  case 4:
/* Line 1787 of yacc.c  */
#line 1021 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 5:
/* Line 1787 of yacc.c  */
#line 1023 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 6:
/* Line 1787 of yacc.c  */
#line 1025 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 7:
/* Line 1787 of yacc.c  */
#line 1027 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 8:
/* Line 1787 of yacc.c  */
#line 1029 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 9:
/* Line 1787 of yacc.c  */
#line 1031 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 10:
/* Line 1787 of yacc.c  */
#line 1033 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 11:
/* Line 1787 of yacc.c  */
#line 1035 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 12:
/* Line 1787 of yacc.c  */
#line 1037 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 13:
/* Line 1787 of yacc.c  */
#line 1039 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 14:
/* Line 1787 of yacc.c  */
#line 1041 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 15:
/* Line 1787 of yacc.c  */
#line 1043 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 16:
/* Line 1787 of yacc.c  */
#line 1045 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 17:
/* Line 1787 of yacc.c  */
#line 1047 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 18:
/* Line 1787 of yacc.c  */
#line 1049 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 19:
/* Line 1787 of yacc.c  */
#line 1051 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 20:
/* Line 1787 of yacc.c  */
#line 1053 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 21:
/* Line 1787 of yacc.c  */
#line 1055 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 22:
/* Line 1787 of yacc.c  */
#line 1057 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 23:
/* Line 1787 of yacc.c  */
#line 1059 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 24:
/* Line 1787 of yacc.c  */
#line 1061 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 25:
/* Line 1787 of yacc.c  */
#line 1063 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 26:
/* Line 1787 of yacc.c  */
#line 1065 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 27:
/* Line 1787 of yacc.c  */
#line 1067 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 28:
/* Line 1787 of yacc.c  */
#line 1069 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 29:
/* Line 1787 of yacc.c  */
#line 1071 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 30:
/* Line 1787 of yacc.c  */
#line 1073 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 31:
/* Line 1787 of yacc.c  */
#line 1080 "ctf-parser.y"
    {	(yyval.gs) = (yyvsp[(1) - (1)].gs);					}
    break;

  case 32:
/* Line 1787 of yacc.c  */
#line 1082 "ctf-parser.y"
    {	(yyval.gs) = gc_string_append(scanner, (yyvsp[(1) - (2)].gs), (yyvsp[(2) - (2)].gs));		}
    break;

  case 33:
/* Line 1787 of yacc.c  */
#line 1087 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;					}
    break;

  case 34:
/* Line 1787 of yacc.c  */
#line 1089 "ctf-parser.y"
    {
			reparent_error(scanner, "escape sequences not supported yet");
		}
    break;

  case 35:
/* Line 1787 of yacc.c  */
#line 1098 "ctf-parser.y"
    {	(yyval.gs) = (yyvsp[(1) - (1)].gs);					}
    break;

  case 36:
/* Line 1787 of yacc.c  */
#line 1100 "ctf-parser.y"
    {	(yyval.gs) = gc_string_append(scanner, (yyvsp[(1) - (2)].gs), (yyvsp[(2) - (2)].gs));		}
    break;

  case 37:
/* Line 1787 of yacc.c  */
#line 1105 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;					}
    break;

  case 38:
/* Line 1787 of yacc.c  */
#line 1107 "ctf-parser.y"
    {
			reparent_error(scanner, "escape sequences not supported yet");
		}
    break;

  case 39:
/* Line 1787 of yacc.c  */
#line 1116 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
		}
    break;

  case 40:
/* Line 1787 of yacc.c  */
#line 1122 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
		}
    break;

  case 41:
/* Line 1787 of yacc.c  */
#line 1128 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
		}
    break;

  case 42:
/* Line 1787 of yacc.c  */
#line 1134 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			sscanf(yylval.gs->s, "%" PRIu64,
			       &(yyval.n)->u.unary_expression.u.unsigned_constant);
		}
    break;

  case 43:
/* Line 1787 of yacc.c  */
#line 1141 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			sscanf(yylval.gs->s, "0%" PRIo64,
			       &(yyval.n)->u.unary_expression.u.unsigned_constant);
		}
    break;

  case 44:
/* Line 1787 of yacc.c  */
#line 1148 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			sscanf(yylval.gs->s, "0x%" PRIx64,
			       &(yyval.n)->u.unary_expression.u.unsigned_constant);
		}
    break;

  case 45:
/* Line 1787 of yacc.c  */
#line 1155 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = "";
		}
    break;

  case 46:
/* Line 1787 of yacc.c  */
#line 1161 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = (yyvsp[(2) - (3)].gs)->s;
		}
    break;

  case 47:
/* Line 1787 of yacc.c  */
#line 1167 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = (yyvsp[(2) - (3)].gs)->s;
		}
    break;

  case 48:
/* Line 1787 of yacc.c  */
#line 1173 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_NESTED;
			(yyval.n)->u.unary_expression.u.nested_exp = (yyvsp[(2) - (3)].n);
		}
    break;

  case 49:
/* Line 1787 of yacc.c  */
#line 1179 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_SBRAC;
			(yyval.n)->u.unary_expression.u.sbrac_exp = (yyvsp[(3) - (4)].n);
			bt_list_splice(&((yyvsp[(1) - (4)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 50:
/* Line 1787 of yacc.c  */
#line 1187 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
			(yyval.n)->u.unary_expression.link = UNARY_DOTLINK;
			bt_list_splice(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 51:
/* Line 1787 of yacc.c  */
#line 1196 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
			(yyval.n)->u.unary_expression.link = UNARY_DOTLINK;
			bt_list_splice(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 52:
/* Line 1787 of yacc.c  */
#line 1205 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
			(yyval.n)->u.unary_expression.link = UNARY_ARROWLINK;
			bt_list_splice(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 53:
/* Line 1787 of yacc.c  */
#line 1214 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
			(yyval.n)->u.unary_expression.link = UNARY_ARROWLINK;
			bt_list_splice(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 54:
/* Line 1787 of yacc.c  */
#line 1226 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);				}
    break;

  case 55:
/* Line 1787 of yacc.c  */
#line 1228 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(2) - (2)].n);				}
    break;

  case 56:
/* Line 1787 of yacc.c  */
#line 1230 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			if ((yyval.n)->u.unary_expression.type != UNARY_SIGNED_CONSTANT
				&& (yyval.n)->u.unary_expression.type != UNARY_UNSIGNED_CONSTANT)
				reparent_error(scanner, "expecting numeric constant");

			if ((yyval.n)->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
				(yyval.n)->u.unary_expression.type = UNARY_SIGNED_CONSTANT;
				(yyval.n)->u.unary_expression.u.signed_constant =
					-((yyval.n)->u.unary_expression.u.unsigned_constant);
			} else {
				(yyval.n)->u.unary_expression.u.signed_constant =
					-((yyval.n)->u.unary_expression.u.signed_constant);
			}
		}
    break;

  case 57:
/* Line 1787 of yacc.c  */
#line 1249 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			(yyvsp[(3) - (3)].n)->u.unary_expression.link = UNARY_DOTDOTDOT;
		}
    break;

  case 58:
/* Line 1787 of yacc.c  */
#line 1255 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);		}
    break;

  case 59:
/* Line 1787 of yacc.c  */
#line 1262 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (2)].n);	}
    break;

  case 60:
/* Line 1787 of yacc.c  */
#line 1264 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 61:
/* Line 1787 of yacc.c  */
#line 1266 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 62:
/* Line 1787 of yacc.c  */
#line 1268 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 63:
/* Line 1787 of yacc.c  */
#line 1270 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 64:
/* Line 1787 of yacc.c  */
#line 1272 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 65:
/* Line 1787 of yacc.c  */
#line 1274 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 66:
/* Line 1787 of yacc.c  */
#line 1276 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(1) - (5)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (5)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(4) - (5)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 67:
/* Line 1787 of yacc.c  */
#line 1287 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 68:
/* Line 1787 of yacc.c  */
#line 1297 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(1) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 69:
/* Line 1787 of yacc.c  */
#line 1307 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEALIAS);
			(yyval.n)->u.typealias.target = make_node(scanner, NODE_TYPEALIAS_TARGET);
			(yyval.n)->u.typealias.alias = make_node(scanner, NODE_TYPEALIAS_ALIAS);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.target->u.typealias_target.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (7)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (7)].n))->tmp_head, &((yyval.n))->u.typealias.target->u.typealias_target.type_declarators);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.alias->u.typealias_alias.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(5) - (7)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(6) - (7)].n))->tmp_head, &((yyval.n))->u.typealias.alias->u.typealias_alias.type_declarators);
		}
    break;

  case 70:
/* Line 1787 of yacc.c  */
#line 1328 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_EVENT);
		}
    break;

  case 71:
/* Line 1787 of yacc.c  */
#line 1332 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_EVENT);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "event_declaration");
		}
    break;

  case 72:
/* Line 1787 of yacc.c  */
#line 1341 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 73:
/* Line 1787 of yacc.c  */
#line 1346 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 74:
/* Line 1787 of yacc.c  */
#line 1352 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STREAM);
		}
    break;

  case 75:
/* Line 1787 of yacc.c  */
#line 1356 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STREAM);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "stream_declaration");
		}
    break;

  case 76:
/* Line 1787 of yacc.c  */
#line 1365 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 77:
/* Line 1787 of yacc.c  */
#line 1370 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 78:
/* Line 1787 of yacc.c  */
#line 1375 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENV);
		}
    break;

  case 79:
/* Line 1787 of yacc.c  */
#line 1379 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENV);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "env declaration");
		}
    break;

  case 80:
/* Line 1787 of yacc.c  */
#line 1388 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 81:
/* Line 1787 of yacc.c  */
#line 1393 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 82:
/* Line 1787 of yacc.c  */
#line 1398 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TRACE);
		}
    break;

  case 83:
/* Line 1787 of yacc.c  */
#line 1402 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TRACE);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
    break;

  case 84:
/* Line 1787 of yacc.c  */
#line 1411 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 85:
/* Line 1787 of yacc.c  */
#line 1416 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 86:
/* Line 1787 of yacc.c  */
#line 1421 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_CLOCK);
		}
    break;

  case 87:
/* Line 1787 of yacc.c  */
#line 1425 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_CLOCK);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
    break;

  case 88:
/* Line 1787 of yacc.c  */
#line 1434 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 89:
/* Line 1787 of yacc.c  */
#line 1439 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 90:
/* Line 1787 of yacc.c  */
#line 1444 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_CALLSITE);
		}
    break;

  case 91:
/* Line 1787 of yacc.c  */
#line 1448 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_CALLSITE);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
    break;

  case 92:
/* Line 1787 of yacc.c  */
#line 1457 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 93:
/* Line 1787 of yacc.c  */
#line 1462 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 94:
/* Line 1787 of yacc.c  */
#line 1467 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 95:
/* Line 1787 of yacc.c  */
#line 1476 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[(1) - (1)].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 96:
/* Line 1787 of yacc.c  */
#line 1484 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 97:
/* Line 1787 of yacc.c  */
#line 1493 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (2)].n);
			bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 98:
/* Line 1787 of yacc.c  */
#line 1501 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 99:
/* Line 1787 of yacc.c  */
#line 1510 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[(1) - (1)].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 100:
/* Line 1787 of yacc.c  */
#line 1518 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 101:
/* Line 1787 of yacc.c  */
#line 1527 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (2)].n);
			bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 102:
/* Line 1787 of yacc.c  */
#line 1535 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 103:
/* Line 1787 of yacc.c  */
#line 1537 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 104:
/* Line 1787 of yacc.c  */
#line 1545 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_CHAR;
		}
    break;

  case 105:
/* Line 1787 of yacc.c  */
#line 1550 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SHORT;
		}
    break;

  case 106:
/* Line 1787 of yacc.c  */
#line 1555 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INT;
		}
    break;

  case 107:
/* Line 1787 of yacc.c  */
#line 1560 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_LONG;
		}
    break;

  case 108:
/* Line 1787 of yacc.c  */
#line 1565 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SIGNED;
		}
    break;

  case 109:
/* Line 1787 of yacc.c  */
#line 1570 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_UNSIGNED;
		}
    break;

  case 110:
/* Line 1787 of yacc.c  */
#line 1575 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_BOOL;
		}
    break;

  case 111:
/* Line 1787 of yacc.c  */
#line 1580 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ID_TYPE;
			(yyval.n)->u.type_specifier.id_type = yylval.gs->s;
		}
    break;

  case 112:
/* Line 1787 of yacc.c  */
#line 1586 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
		}
    break;

  case 113:
/* Line 1787 of yacc.c  */
#line 1592 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "integer reparent error");
		}
    break;

  case 114:
/* Line 1787 of yacc.c  */
#line 1603 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_VOID;
		}
    break;

  case 115:
/* Line 1787 of yacc.c  */
#line 1608 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_CHAR;
		}
    break;

  case 116:
/* Line 1787 of yacc.c  */
#line 1613 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SHORT;
		}
    break;

  case 117:
/* Line 1787 of yacc.c  */
#line 1618 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INT;
		}
    break;

  case 118:
/* Line 1787 of yacc.c  */
#line 1623 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_LONG;
		}
    break;

  case 119:
/* Line 1787 of yacc.c  */
#line 1628 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOAT;
		}
    break;

  case 120:
/* Line 1787 of yacc.c  */
#line 1633 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_DOUBLE;
		}
    break;

  case 121:
/* Line 1787 of yacc.c  */
#line 1638 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SIGNED;
		}
    break;

  case 122:
/* Line 1787 of yacc.c  */
#line 1643 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_UNSIGNED;
		}
    break;

  case 123:
/* Line 1787 of yacc.c  */
#line 1648 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_BOOL;
		}
    break;

  case 124:
/* Line 1787 of yacc.c  */
#line 1653 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_COMPLEX;
		}
    break;

  case 125:
/* Line 1787 of yacc.c  */
#line 1658 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_IMAGINARY;
		}
    break;

  case 126:
/* Line 1787 of yacc.c  */
#line 1663 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ID_TYPE;
			(yyval.n)->u.type_specifier.id_type = yylval.gs->s;
		}
    break;

  case 127:
/* Line 1787 of yacc.c  */
#line 1669 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_FLOATING_POINT);
		}
    break;

  case 128:
/* Line 1787 of yacc.c  */
#line 1675 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_FLOATING_POINT);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "floating point reparent error");
		}
    break;

  case 129:
/* Line 1787 of yacc.c  */
#line 1683 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
		}
    break;

  case 130:
/* Line 1787 of yacc.c  */
#line 1689 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "integer reparent error");
		}
    break;

  case 131:
/* Line 1787 of yacc.c  */
#line 1697 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
		}
    break;

  case 132:
/* Line 1787 of yacc.c  */
#line 1703 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
		}
    break;

  case 133:
/* Line 1787 of yacc.c  */
#line 1709 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "string reparent error");
		}
    break;

  case 134:
/* Line 1787 of yacc.c  */
#line 1717 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ENUM;
			(yyval.n)->u.type_specifier.node = (yyvsp[(2) - (2)].n);
		}
    break;

  case 135:
/* Line 1787 of yacc.c  */
#line 1723 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_VARIANT;
			(yyval.n)->u.type_specifier.node = (yyvsp[(2) - (2)].n);
		}
    break;

  case 136:
/* Line 1787 of yacc.c  */
#line 1729 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRUCT;
			(yyval.n)->u.type_specifier.node = (yyvsp[(2) - (2)].n);
		}
    break;

  case 137:
/* Line 1787 of yacc.c  */
#line 1738 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			if ((yyvsp[(2) - (3)].n) && set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 138:
/* Line 1787 of yacc.c  */
#line 1745 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 139:
/* Line 1787 of yacc.c  */
#line 1753 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 140:
/* Line 1787 of yacc.c  */
#line 1761 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 0;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 141:
/* Line 1787 of yacc.c  */
#line 1767 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 0;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 142:
/* Line 1787 of yacc.c  */
#line 1773 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			bt_list_add_tail(&((yyvsp[(6) - (7)].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[(2) - (7)].n) && set_parent_node((yyvsp[(2) - (7)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 143:
/* Line 1787 of yacc.c  */
#line 1781 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (8)].gs)->s;
			bt_list_add_tail(&((yyvsp[(7) - (8)].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[(3) - (8)].n) && set_parent_node((yyvsp[(3) - (8)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 144:
/* Line 1787 of yacc.c  */
#line 1790 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (8)].gs)->s;
			bt_list_add_tail(&((yyvsp[(7) - (8)].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[(3) - (8)].n) && set_parent_node((yyvsp[(3) - (8)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 145:
/* Line 1787 of yacc.c  */
#line 1802 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 146:
/* Line 1787 of yacc.c  */
#line 1807 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 147:
/* Line 1787 of yacc.c  */
#line 1812 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			if ((yyvsp[(2) - (3)].n) && set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 148:
/* Line 1787 of yacc.c  */
#line 1819 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.choice = (yyvsp[(2) - (6)].gs)->s;
			if ((yyvsp[(5) - (6)].n) && set_parent_node((yyvsp[(5) - (6)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 149:
/* Line 1787 of yacc.c  */
#line 1827 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.choice = (yyvsp[(2) - (6)].gs)->s;
			if ((yyvsp[(5) - (6)].n) && set_parent_node((yyvsp[(5) - (6)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 150:
/* Line 1787 of yacc.c  */
#line 1835 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 151:
/* Line 1787 of yacc.c  */
#line 1843 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 152:
/* Line 1787 of yacc.c  */
#line 1852 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 153:
/* Line 1787 of yacc.c  */
#line 1859 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 154:
/* Line 1787 of yacc.c  */
#line 1868 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 155:
/* Line 1787 of yacc.c  */
#line 1875 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 156:
/* Line 1787 of yacc.c  */
#line 1883 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 157:
/* Line 1787 of yacc.c  */
#line 1892 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 158:
/* Line 1787 of yacc.c  */
#line 1899 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 159:
/* Line 1787 of yacc.c  */
#line 1908 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 160:
/* Line 1787 of yacc.c  */
#line 1918 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 161:
/* Line 1787 of yacc.c  */
#line 1923 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 162:
/* Line 1787 of yacc.c  */
#line 1928 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			_bt_list_splice_tail(&((yyvsp[(2) - (3)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 163:
/* Line 1787 of yacc.c  */
#line 1934 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			((yyval.n))->u._enum.container_type = (yyvsp[(2) - (5)].n);
			_bt_list_splice_tail(&((yyvsp[(4) - (5)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 164:
/* Line 1787 of yacc.c  */
#line 1941 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (4)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 165:
/* Line 1787 of yacc.c  */
#line 1948 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (6)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (6)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (6)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 166:
/* Line 1787 of yacc.c  */
#line 1956 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (4)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 167:
/* Line 1787 of yacc.c  */
#line 1963 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (6)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (6)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (6)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 168:
/* Line 1787 of yacc.c  */
#line 1971 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			_bt_list_splice_tail(&((yyvsp[(2) - (4)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 169:
/* Line 1787 of yacc.c  */
#line 1977 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			((yyval.n))->u._enum.container_type = (yyvsp[(2) - (6)].n);
			_bt_list_splice_tail(&((yyvsp[(4) - (6)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 170:
/* Line 1787 of yacc.c  */
#line 1984 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (5)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (5)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 171:
/* Line 1787 of yacc.c  */
#line 1991 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (7)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (7)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (7)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 172:
/* Line 1787 of yacc.c  */
#line 1999 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 0;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 173:
/* Line 1787 of yacc.c  */
#line 2005 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (5)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (5)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 174:
/* Line 1787 of yacc.c  */
#line 2012 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (7)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (7)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (7)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 175:
/* Line 1787 of yacc.c  */
#line 2020 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 0;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 176:
/* Line 1787 of yacc.c  */
#line 2029 "ctf-parser.y"
    {	(yyval.n) = NULL;	}
    break;

  case 177:
/* Line 1787 of yacc.c  */
#line 2031 "ctf-parser.y"
    {
			if ((yyvsp[(1) - (2)].n)) {
				(yyval.n) = (yyvsp[(1) - (2)].n);
				bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->tmp_head);
			} else {
				(yyval.n) = (yyvsp[(2) - (2)].n);
				bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
			}
		}
    break;

  case 178:
/* Line 1787 of yacc.c  */
#line 2044 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (3)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_STRUCT_OR_VARIANT_DECLARATION);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (3)].n))->tmp_head, &((yyval.n))->u.struct_or_variant_declaration.type_declarators);
		}
    break;

  case 179:
/* Line 1787 of yacc.c  */
#line 2054 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(1) - (5)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (5)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(4) - (5)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 180:
/* Line 1787 of yacc.c  */
#line 2065 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 181:
/* Line 1787 of yacc.c  */
#line 2075 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 182:
/* Line 1787 of yacc.c  */
#line 2085 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEALIAS);
			(yyval.n)->u.typealias.target = make_node(scanner, NODE_TYPEALIAS_TARGET);
			(yyval.n)->u.typealias.alias = make_node(scanner, NODE_TYPEALIAS_ALIAS);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.target->u.typealias_target.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (7)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (7)].n))->tmp_head, &((yyval.n))->u.typealias.target->u.typealias_target.type_declarators);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.alias->u.typealias_alias.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(5) - (7)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(6) - (7)].n))->tmp_head, &((yyval.n))->u.typealias.alias->u.typealias_alias.type_declarators);
		}
    break;

  case 183:
/* Line 1787 of yacc.c  */
#line 2106 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 184:
/* Line 1787 of yacc.c  */
#line 2115 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[(1) - (1)].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 185:
/* Line 1787 of yacc.c  */
#line 2123 "ctf-parser.y"
    {
			struct ctf_node *node;

			add_type(scanner, (yyvsp[(1) - (1)].gs));
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_ID_TYPE;
			node->u.type_specifier.id_type = yylval.gs->s;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 186:
/* Line 1787 of yacc.c  */
#line 2134 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 187:
/* Line 1787 of yacc.c  */
#line 2143 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (2)].n);
			bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 188:
/* Line 1787 of yacc.c  */
#line 2148 "ctf-parser.y"
    {
			struct ctf_node *node;

			add_type(scanner, (yyvsp[(2) - (2)].gs));
			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_ID_TYPE;
			node->u.type_specifier.id_type = yylval.gs->s;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 189:
/* Line 1787 of yacc.c  */
#line 2162 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 190:
/* Line 1787 of yacc.c  */
#line 2164 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 191:
/* Line 1787 of yacc.c  */
#line 2172 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 192:
/* Line 1787 of yacc.c  */
#line 2174 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(2) - (2)].n);	}
    break;

  case 193:
/* Line 1787 of yacc.c  */
#line 2176 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			if (set_parent_node((yyvsp[(3) - (3)].n), (yyvsp[(1) - (3)].n)))
				reparent_error(scanner, "struct_or_variant_declarator");
		}
    break;

  case 194:
/* Line 1787 of yacc.c  */
#line 2185 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 195:
/* Line 1787 of yacc.c  */
#line 2187 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 196:
/* Line 1787 of yacc.c  */
#line 2195 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 197:
/* Line 1787 of yacc.c  */
#line 2200 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 198:
/* Line 1787 of yacc.c  */
#line 2205 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 199:
/* Line 1787 of yacc.c  */
#line 2210 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = "";
		}
    break;

  case 200:
/* Line 1787 of yacc.c  */
#line 2215 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(2) - (3)].gs)->s;
		}
    break;

  case 201:
/* Line 1787 of yacc.c  */
#line 2220 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (3)].gs)->s;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 202:
/* Line 1787 of yacc.c  */
#line 2226 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (3)].gs)->s;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 203:
/* Line 1787 of yacc.c  */
#line 2232 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (3)].gs)->s;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 204:
/* Line 1787 of yacc.c  */
#line 2238 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = "";
			bt_list_splice(&((yyvsp[(4) - (4)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 205:
/* Line 1787 of yacc.c  */
#line 2244 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(2) - (5)].gs)->s;
			bt_list_splice(&((yyvsp[(5) - (5)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 206:
/* Line 1787 of yacc.c  */
#line 2253 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 207:
/* Line 1787 of yacc.c  */
#line 2255 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 208:
/* Line 1787 of yacc.c  */
#line 2263 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 209:
/* Line 1787 of yacc.c  */
#line 2265 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 210:
/* Line 1787 of yacc.c  */
#line 2273 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
                        (yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			/* id is NULL */
		}
    break;

  case 211:
/* Line 1787 of yacc.c  */
#line 2279 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 212:
/* Line 1787 of yacc.c  */
#line 2285 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 213:
/* Line 1787 of yacc.c  */
#line 2291 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 214:
/* Line 1787 of yacc.c  */
#line 2299 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (3)].n);
			(yyval.n)->u.type_declarator.u.nested.abstract_array = 1;
		}
    break;

  case 215:
/* Line 1787 of yacc.c  */
#line 2309 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 216:
/* Line 1787 of yacc.c  */
#line 2311 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 217:
/* Line 1787 of yacc.c  */
#line 2319 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 218:
/* Line 1787 of yacc.c  */
#line 2321 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 219:
/* Line 1787 of yacc.c  */
#line 2329 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
                        (yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			/* id is NULL */
		}
    break;

  case 220:
/* Line 1787 of yacc.c  */
#line 2335 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 221:
/* Line 1787 of yacc.c  */
#line 2341 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 222:
/* Line 1787 of yacc.c  */
#line 2349 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (3)].n);
			(yyval.n)->u.type_declarator.u.nested.abstract_array = 1;
		}
    break;

  case 223:
/* Line 1787 of yacc.c  */
#line 2359 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 224:
/* Line 1787 of yacc.c  */
#line 2361 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 225:
/* Line 1787 of yacc.c  */
#line 2369 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 226:
/* Line 1787 of yacc.c  */
#line 2375 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 227:
/* Line 1787 of yacc.c  */
#line 2381 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 228:
/* Line 1787 of yacc.c  */
#line 2392 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 229:
/* Line 1787 of yacc.c  */
#line 2394 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 230:
/* Line 1787 of yacc.c  */
#line 2402 "ctf-parser.y"
    {
			add_type(scanner, (yyvsp[(1) - (1)].gs));
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 231:
/* Line 1787 of yacc.c  */
#line 2409 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 232:
/* Line 1787 of yacc.c  */
#line 2415 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 233:
/* Line 1787 of yacc.c  */
#line 2426 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
		}
    break;

  case 234:
/* Line 1787 of yacc.c  */
#line 2430 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
			bt_list_splice(&((yyvsp[(2) - (2)].n))->tmp_head, &((yyval.n))->tmp_head);
		}
    break;

  case 235:
/* Line 1787 of yacc.c  */
#line 2435 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
			(yyval.n)->u.pointer.const_qualifier = 1;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
		}
    break;

  case 238:
/* Line 1787 of yacc.c  */
#line 2452 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (2)].n);	}
    break;

  case 239:
/* Line 1787 of yacc.c  */
#line 2454 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(2) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 240:
/* Line 1787 of yacc.c  */
#line 2462 "ctf-parser.y"
    {
			/*
			 * Because we have left and right, cannot use
			 * set_parent_node.
			 */
			(yyval.n) = make_node(scanner, NODE_CTF_EXPRESSION);
			_bt_list_splice_tail(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->u.ctf_expression.left);
			if ((yyvsp[(1) - (3)].n)->u.unary_expression.type != UNARY_STRING)
				reparent_error(scanner, "ctf_assignment_expression left expects string");
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.ctf_expression.right);
		}
    break;

  case 241:
/* Line 1787 of yacc.c  */
#line 2474 "ctf-parser.y"
    {
			/*
			 * Because we have left and right, cannot use
			 * set_parent_node.
			 */
			(yyval.n) = make_node(scanner, NODE_CTF_EXPRESSION);
			_bt_list_splice_tail(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->u.ctf_expression.left);
			if ((yyvsp[(1) - (3)].n)->u.unary_expression.type != UNARY_STRING)
				reparent_error(scanner, "ctf_assignment_expression left expects string");
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->u.ctf_expression.right);
		}
    break;

  case 242:
/* Line 1787 of yacc.c  */
#line 2486 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(4) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 243:
/* Line 1787 of yacc.c  */
#line 2497 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (3)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 244:
/* Line 1787 of yacc.c  */
#line 2507 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (3)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 245:
/* Line 1787 of yacc.c  */
#line 2517 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEALIAS);
			(yyval.n)->u.typealias.target = make_node(scanner, NODE_TYPEALIAS_TARGET);
			(yyval.n)->u.typealias.alias = make_node(scanner, NODE_TYPEALIAS_ALIAS);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.target->u.typealias_target.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (6)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (6)].n))->tmp_head, &((yyval.n))->u.typealias.target->u.typealias_target.type_declarators);

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u.typealias.alias->u.typealias_alias.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(5) - (6)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(6) - (6)].n))->tmp_head, &((yyval.n))->u.typealias.alias->u.typealias_alias.type_declarators);
		}
    break;


/* Line 1787 of yacc.c  */
#line 5544 "ctf-parser.c"
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

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (scanner, YY_("syntax error"));
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
        yyerror (scanner, yymsgp);
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
		      yytoken, &yylval, scanner);
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

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

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
		  yystos[yystate], yyvsp, scanner);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


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
  yyerror (scanner, YY_("memory exhausted"));
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
                  yytoken, &yylval, scanner);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, scanner);
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
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


