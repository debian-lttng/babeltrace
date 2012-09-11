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
	return ast;
}

static void ctf_ast_free(struct ctf_ast *ast)
{
	struct ctf_node *node, *tmp;

	bt_list_for_each_entry_safe(node, tmp, &ast->allocated_nodes, gc)
		free(node);
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
#line 969 "ctf-parser.c"

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
     CLOCK = 300,
     TYPEALIAS = 301,
     TYPEDEF = 302,
     UNSIGNED = 303,
     VARIANT = 304,
     VOID = 305,
     _BOOL = 306,
     _COMPLEX = 307,
     _IMAGINARY = 308,
     DECIMAL_CONSTANT = 309,
     OCTAL_CONSTANT = 310,
     HEXADECIMAL_CONSTANT = 311,
     TOK_ALIGN = 312,
     IDENTIFIER = 313,
     ID_TYPE = 314,
     ERROR = 315
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
#define CLOCK 300
#define TYPEALIAS 301
#define TYPEDEF 302
#define UNSIGNED 303
#define VARIANT 304
#define VOID 305
#define _BOOL 306
#define _COMPLEX 307
#define _IMAGINARY 308
#define DECIMAL_CONSTANT 309
#define OCTAL_CONSTANT 310
#define HEXADECIMAL_CONSTANT 311
#define TOK_ALIGN 312
#define IDENTIFIER 313
#define ID_TYPE 314
#define ERROR 315



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 350 of yacc.c  */
#line 920 "ctf-parser.y"

	long long ll;
	char c;
	struct gc_string *gs;
	struct ctf_node *n;


/* Line 350 of yacc.c  */
#line 1140 "ctf-parser.c"
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
#line 1167 "ctf-parser.c"

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
#define YYFINAL  68
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2196

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  61
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  59
/* YYNRULES -- Number of rules.  */
#define YYNRULES  239
/* YYNRULES -- Number of states.  */
#define YYNSTATES  453

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   315

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
      55,    56,    57,    58,    59,    60
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    14,    16,    18,
      20,    22,    24,    26,    28,    30,    32,    34,    36,    38,
      40,    42,    44,    46,    48,    50,    52,    54,    56,    58,
      60,    62,    65,    67,    69,    71,    74,    76,    78,    80,
      82,    84,    86,    88,    90,    93,    97,   101,   105,   110,
     114,   118,   122,   126,   128,   131,   134,   138,   140,   143,
     145,   147,   149,   151,   153,   159,   164,   169,   177,   180,
     184,   187,   190,   193,   197,   200,   203,   206,   210,   213,
     216,   219,   223,   226,   229,   233,   238,   240,   243,   245,
     247,   250,   253,   255,   257,   260,   263,   265,   269,   271,
     273,   275,   277,   279,   281,   283,   285,   289,   294,   296,
     298,   300,   302,   304,   306,   308,   310,   312,   314,   316,
     318,   320,   324,   329,   333,   338,   340,   344,   349,   352,
     355,   358,   362,   367,   372,   374,   376,   384,   393,   402,
     404,   406,   410,   417,   424,   429,   437,   442,   450,   455,
     460,   468,   473,   481,   486,   488,   490,   494,   500,   505,
     512,   517,   524,   529,   536,   542,   550,   552,   558,   566,
     568,   569,   572,   576,   582,   587,   592,   600,   602,   604,
     606,   609,   612,   615,   617,   621,   623,   626,   630,   632,
     636,   638,   640,   642,   645,   649,   653,   657,   661,   666,
     672,   674,   678,   680,   683,   684,   686,   690,   695,   699,
     701,   705,   707,   710,   711,   715,   720,   724,   726,   729,
     731,   735,   740,   742,   745,   747,   751,   756,   758,   761,
     765,   767,   770,   773,   777,   781,   785,   790,   794,   798
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      62,     0,    -1,    71,    -1,    62,    71,    -1,    50,    -1,
      29,    -1,    39,    -1,    37,    -1,    38,    -1,    35,    -1,
      30,    -1,    40,    -1,    48,    -1,    51,    -1,    52,    -1,
      53,    -1,    34,    -1,    36,    -1,    42,    -1,    31,    -1,
      49,    -1,    43,    -1,    28,    -1,    47,    -1,    33,    -1,
      41,    -1,    32,    -1,    44,    -1,    45,    -1,    57,    -1,
      65,    -1,    64,    65,    -1,     8,    -1,     7,    -1,    67,
      -1,    66,    67,    -1,     8,    -1,     7,    -1,    58,    -1,
      59,    -1,    63,    -1,    54,    -1,    55,    -1,    56,    -1,
       5,     6,    -1,     5,    66,     6,    -1,     3,    64,     4,
      -1,    11,    69,    12,    -1,    68,     9,    69,    10,    -1,
      68,    25,    58,    -1,    68,    25,    59,    -1,    68,    15,
      58,    -1,    68,    15,    59,    -1,    68,    -1,    17,    68,
      -1,    18,    68,    -1,    69,    24,    69,    -1,    69,    -1,
      88,    23,    -1,    72,    -1,    75,    -1,    78,    -1,    81,
      -1,    84,    -1,    88,    47,    88,    89,    23,    -1,    47,
      88,    89,    23,    -1,    88,    47,    89,    23,    -1,    46,
      88,   106,    21,   101,   109,    23,    -1,    73,    74,    -1,
      73,   118,    74,    -1,    33,    13,    -1,    14,    23,    -1,
      76,    77,    -1,    76,   118,    77,    -1,    41,    13,    -1,
      14,    23,    -1,    79,    80,    -1,    79,   118,    80,    -1,
      32,    13,    -1,    14,    23,    -1,    82,    83,    -1,    82,
     118,    83,    -1,    44,    13,    -1,    14,    23,    -1,    45,
      85,    86,    -1,    45,    85,   118,    86,    -1,    13,    -1,
      14,    23,    -1,    28,    -1,    90,    -1,    87,    28,    -1,
      87,    90,    -1,    28,    -1,    91,    -1,    88,    28,    -1,
      88,    91,    -1,   114,    -1,    89,    27,   114,    -1,    29,
      -1,    39,    -1,    37,    -1,    38,    -1,    40,    -1,    48,
      -1,    51,    -1,    59,    -1,    36,    13,    14,    -1,    36,
      13,   118,    14,    -1,    50,    -1,    29,    -1,    39,    -1,
      37,    -1,    38,    -1,    35,    -1,    30,    -1,    40,    -1,
      48,    -1,    51,    -1,    52,    -1,    53,    -1,    59,    -1,
      34,    13,    14,    -1,    34,    13,   118,    14,    -1,    36,
      13,    14,    -1,    36,    13,   118,    14,    -1,    42,    -1,
      42,    13,    14,    -1,    42,    13,   118,    14,    -1,    31,
      98,    -1,    49,    95,    -1,    43,    92,    -1,    93,    99,
      94,    -1,    58,    93,    99,    94,    -1,    59,    93,    99,
      94,    -1,    58,    -1,    59,    -1,    93,    99,    94,    57,
      11,    69,    12,    -1,    58,    93,    99,    94,    57,    11,
      69,    12,    -1,    59,    93,    99,    94,    57,    11,    69,
      12,    -1,    13,    -1,    14,    -1,    96,    99,    97,    -1,
      19,    58,    20,    96,    99,    97,    -1,    19,    59,    20,
      96,    99,    97,    -1,    58,    96,    99,    97,    -1,    58,
      19,    58,    20,    96,    99,    97,    -1,    58,    19,    58,
      20,    -1,    58,    19,    59,    20,    96,    99,    97,    -1,
      58,    19,    59,    20,    -1,    59,    96,    99,    97,    -1,
      59,    19,    58,    20,    96,    99,    97,    -1,    59,    19,
      58,    20,    -1,    59,    19,    59,    20,    96,    99,    97,
      -1,    59,    19,    59,    20,    -1,    13,    -1,    14,    -1,
      13,   104,    14,    -1,    22,    87,    13,   104,    14,    -1,
      58,    13,   104,    14,    -1,    58,    22,    87,    13,   104,
      14,    -1,    59,    13,   104,    14,    -1,    59,    22,    87,
      13,   104,    14,    -1,    13,   104,    27,    14,    -1,    22,
      87,    13,   104,    27,    14,    -1,    58,    13,   104,    27,
      14,    -1,    58,    22,    87,    13,   104,    27,    14,    -1,
      58,    -1,    59,    13,   104,    27,    14,    -1,    59,    22,
      87,    13,   104,    27,    14,    -1,    59,    -1,    -1,    99,
     100,    -1,    88,   102,    23,    -1,    88,    47,    88,    89,
      23,    -1,    47,    88,    89,    23,    -1,    88,    47,    89,
      23,    -1,    46,    88,   106,    21,   101,   109,    23,    -1,
      28,    -1,    91,    -1,    58,    -1,   101,    28,    -1,   101,
      91,    -1,   101,    58,    -1,   103,    -1,   102,    27,   103,
      -1,   112,    -1,    22,    69,    -1,   112,    22,    69,    -1,
     105,    -1,   104,    27,   105,    -1,    58,    -1,    59,    -1,
      63,    -1,     5,     6,    -1,     5,    66,     6,    -1,    58,
      26,    70,    -1,    59,    26,    70,    -1,    63,    26,    70,
      -1,     5,     6,    26,    70,    -1,     5,    66,     6,    26,
      70,    -1,   107,    -1,   106,    27,   107,    -1,   108,    -1,
     116,   108,    -1,    -1,    58,    -1,    11,   107,    12,    -1,
     108,     9,    69,    10,    -1,   108,     9,    10,    -1,   110,
      -1,   109,    27,   110,    -1,   111,    -1,   116,   111,    -1,
      -1,    11,   110,    12,    -1,   111,     9,    69,    10,    -1,
     111,     9,    10,    -1,   113,    -1,   116,   113,    -1,    58,
      -1,    11,   112,    12,    -1,   113,     9,    69,    10,    -1,
     115,    -1,   116,   115,    -1,    58,    -1,    11,   114,    12,
      -1,   115,     9,    69,    10,    -1,    16,    -1,    16,   116,
      -1,    16,   117,   116,    -1,    28,    -1,   117,    28,    -1,
     119,    23,    -1,   118,   119,    23,    -1,    69,    26,    69,
      -1,    69,    21,    88,    -1,    88,    47,    88,    89,    -1,
      47,    88,    89,    -1,    88,    47,    89,    -1,    46,    88,
     106,    21,   101,   109,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   971,   971,   976,   984,   986,   988,   990,   992,   994,
     996,   998,  1000,  1002,  1004,  1006,  1008,  1010,  1012,  1014,
    1016,  1018,  1020,  1022,  1024,  1026,  1028,  1030,  1032,  1034,
    1041,  1043,  1048,  1050,  1059,  1061,  1066,  1068,  1077,  1083,
    1089,  1095,  1102,  1109,  1116,  1122,  1128,  1134,  1140,  1148,
    1157,  1166,  1175,  1187,  1189,  1191,  1210,  1216,  1223,  1225,
    1227,  1229,  1231,  1233,  1235,  1246,  1256,  1266,  1287,  1291,
    1300,  1305,  1311,  1315,  1324,  1329,  1334,  1338,  1347,  1352,
    1357,  1361,  1370,  1375,  1380,  1384,  1393,  1398,  1403,  1412,
    1420,  1429,  1437,  1446,  1454,  1463,  1471,  1473,  1481,  1486,
    1491,  1496,  1501,  1506,  1511,  1516,  1522,  1528,  1539,  1544,
    1549,  1554,  1559,  1564,  1569,  1574,  1579,  1584,  1589,  1594,
    1599,  1605,  1611,  1619,  1625,  1633,  1639,  1645,  1653,  1659,
    1665,  1674,  1681,  1689,  1697,  1703,  1709,  1717,  1726,  1738,
    1743,  1748,  1755,  1763,  1771,  1779,  1788,  1795,  1804,  1811,
    1819,  1828,  1835,  1844,  1854,  1859,  1864,  1870,  1877,  1884,
    1892,  1899,  1907,  1913,  1920,  1927,  1935,  1941,  1948,  1956,
    1966,  1967,  1980,  1990,  2001,  2011,  2021,  2042,  2051,  2059,
    2070,  2079,  2084,  2098,  2100,  2108,  2110,  2112,  2121,  2123,
    2131,  2136,  2141,  2146,  2151,  2156,  2162,  2168,  2174,  2180,
    2189,  2191,  2199,  2201,  2210,  2215,  2221,  2227,  2235,  2245,
    2247,  2255,  2257,  2266,  2271,  2277,  2285,  2295,  2297,  2305,
    2311,  2317,  2328,  2330,  2338,  2345,  2351,  2362,  2366,  2371,
    2381,  2382,  2388,  2390,  2398,  2410,  2422,  2433,  2443,  2453
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
  "SHORT", "SIGNED", "STREAM", "STRING", "STRUCT", "TRACE", "CLOCK",
  "TYPEALIAS", "TYPEDEF", "UNSIGNED", "VARIANT", "VOID", "_BOOL",
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
  "integer_declaration_specifiers", "declaration_specifiers",
  "type_declarator_list", "integer_type_specifier", "type_specifier",
  "struct_type_specifier", "struct_declaration_begin",
  "struct_declaration_end", "variant_type_specifier",
  "variant_declaration_begin", "variant_declaration_end",
  "enum_type_specifier", "struct_or_variant_declaration_list",
  "struct_or_variant_declaration", "alias_declaration_specifiers",
  "struct_or_variant_declarator_list", "struct_or_variant_declarator",
  "enumerator_list", "enumerator", "abstract_declarator_list",
  "abstract_declarator", "direct_abstract_declarator",
  "alias_abstract_declarator_list", "alias_abstract_declarator",
  "direct_alias_abstract_declarator", "declarator", "direct_declarator",
  "type_declarator", "direct_type_declarator", "pointer",
  "type_qualifier_list", "ctf_assignment_expression_list",
  "ctf_assignment_expression", YY_NULL
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
     315
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    61,    62,    62,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      64,    64,    65,    65,    66,    66,    67,    67,    68,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    68,    68,
      68,    68,    68,    69,    69,    69,    70,    70,    71,    71,
      71,    71,    71,    71,    71,    71,    71,    71,    72,    72,
      73,    74,    75,    75,    76,    77,    78,    78,    79,    80,
      81,    81,    82,    83,    84,    84,    85,    86,    87,    87,
      87,    87,    88,    88,    88,    88,    89,    89,    90,    90,
      90,    90,    90,    90,    90,    90,    90,    90,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    92,    92,    92,    92,    92,    92,    92,    92,    93,
      94,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    96,    97,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      99,    99,   100,   100,   100,   100,   100,   101,   101,   101,
     101,   101,   101,   102,   102,   103,   103,   103,   104,   104,
     105,   105,   105,   105,   105,   105,   105,   105,   105,   105,
     106,   106,   107,   107,   108,   108,   108,   108,   108,   109,
     109,   110,   110,   111,   111,   111,   111,   112,   112,   113,
     113,   113,   114,   114,   115,   115,   115,   116,   116,   116,
     117,   117,   118,   118,   119,   119,   119,   119,   119,   119
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     3,     3,     3,     4,     3,
       3,     3,     3,     1,     2,     2,     3,     1,     2,     1,
       1,     1,     1,     1,     5,     4,     4,     7,     2,     3,
       2,     2,     2,     3,     2,     2,     2,     3,     2,     2,
       2,     3,     2,     2,     3,     4,     1,     2,     1,     1,
       2,     2,     1,     1,     2,     2,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     4,     3,     4,     1,     3,     4,     2,     2,
       2,     3,     4,     4,     1,     1,     7,     8,     8,     1,
       1,     3,     6,     6,     4,     7,     4,     7,     4,     4,
       7,     4,     7,     4,     1,     1,     3,     5,     4,     6,
       4,     6,     4,     6,     5,     7,     1,     5,     7,     1,
       0,     2,     3,     5,     4,     4,     7,     1,     1,     1,
       2,     2,     2,     1,     3,     1,     2,     3,     1,     3,
       1,     1,     1,     2,     3,     3,     3,     3,     4,     5,
       1,     3,     1,     2,     0,     1,     3,     4,     3,     1,
       3,     1,     2,     0,     3,     4,     3,     1,     2,     1,
       3,     4,     1,     2,     1,     3,     4,     1,     2,     3,
       1,     2,     2,     3,     3,     3,     4,     3,     3,     6
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    92,   109,   114,     0,     0,     0,     0,   113,     0,
     111,   112,   110,   115,     0,   125,     0,     0,     0,     0,
       0,   116,     0,   108,   117,   118,   119,   120,     0,     2,
      59,     0,    60,     0,    61,     0,    62,     0,    63,     0,
      93,     0,     0,   166,   169,   128,    78,    70,     0,     0,
      74,     0,   139,   134,   135,   130,   170,    82,    86,     0,
     204,     0,   154,     0,     0,     0,   129,   170,     1,     3,
       0,     0,     0,     0,     0,     0,    92,   109,   114,    19,
      26,    24,    16,   113,    17,   111,   112,   110,   115,    25,
     125,    21,    27,    28,     0,    23,   116,    20,   108,   117,
     118,   119,    41,    42,    43,    29,    38,   120,    40,    53,
       0,    68,     0,     0,     0,     0,    72,     0,     0,    76,
       0,     0,    80,     0,    58,    94,     0,    95,     0,    22,
       5,    10,    19,    16,     9,    17,     7,     8,     6,    11,
      18,    21,    23,    12,    20,     4,    13,    14,    15,   190,
     191,   192,     0,   188,    88,    98,     0,   100,   101,    99,
     102,   103,   104,   105,     0,    89,     0,     0,     0,     0,
     121,     0,   123,     0,   126,     0,   170,   170,     0,     0,
      84,     0,   204,   227,   205,     0,   200,   202,   204,     0,
     224,     0,    96,   222,     0,     0,     0,     0,   170,     0,
     170,     0,    33,    32,     0,    30,    44,    37,    36,     0,
      34,    39,     0,    71,    54,    55,   204,     0,     0,     0,
       0,     0,     0,     0,    69,     0,   232,    75,    73,    79,
      77,    83,    81,     0,     0,   193,     0,     0,     0,     0,
     156,     0,     0,     0,    90,    91,     0,     0,     0,     0,
     122,   124,   127,     0,     0,   140,     0,     0,     0,   131,
     171,    87,    85,     0,   230,   228,     0,     0,   204,     0,
     203,     0,    65,     0,     0,   223,     0,     0,     0,     0,
       0,     0,     0,     0,   155,   141,    46,    31,    45,    35,
      47,     0,   237,     0,    51,    52,    49,    50,   235,   234,
       0,   238,   233,     0,    66,     0,   194,    57,   195,   196,
     197,   162,   189,   106,     0,     0,   158,     0,     0,   160,
       0,     0,   132,   133,   204,     0,     0,     0,     0,   219,
       0,   183,   185,   217,     0,     0,   206,   231,   229,   177,
     179,   178,   213,   201,   208,     0,   225,    97,     0,   170,
     170,   146,   148,   144,   151,   153,   149,     0,    48,   236,
      64,   198,     0,     0,   107,   157,     0,   164,     0,   167,
       0,     0,     0,     0,     0,     0,   186,     0,     0,   172,
       0,     0,     0,   218,     0,   213,   180,   182,   181,     0,
     209,   211,   213,   207,   226,     0,     0,   170,   170,   170,
     170,   213,   199,    56,   163,   159,     0,   161,     0,     0,
       0,     0,   174,   220,     0,   175,   184,   187,     0,     0,
       0,    67,   213,     0,   212,   142,   143,     0,     0,     0,
       0,   239,   165,   168,     0,     0,   213,   173,   221,   136,
     214,   210,   216,     0,   145,   147,   150,   152,   137,   138,
       0,   215,   176
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    28,   108,   204,   205,   209,   210,   109,   110,   308,
      29,    30,    31,   111,    32,    33,   116,    34,    35,   119,
      36,    37,   122,    38,    59,   180,   164,   112,   191,   165,
      40,    55,    56,   259,    66,    67,   285,    45,   178,   260,
     342,   330,   331,   152,   153,   185,   186,   187,   389,   390,
     391,   332,   333,   192,   193,   194,   266,   113,   114
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -365
static const yytype_int16 yypact[] =
{
    2033,  -365,  -365,  -365,    30,    40,    52,    55,  -365,    62,
    -365,  -365,  -365,  -365,    93,   102,    79,   105,   127,  2111,
    2111,  -365,    45,  -365,  -365,  -365,  -365,  -365,   403,  -365,
    -365,   454,  -365,   511,  -365,   568,  -365,   625,  -365,  2001,
    -365,  1667,   116,    38,    69,  -365,  -365,  -365,   682,   739,
    -365,   796,  -365,   137,   137,  -365,  -365,  -365,  -365,   853,
    1755,  1799,  -365,   187,    97,   163,  -365,  -365,  -365,  -365,
     271,   244,  1309,    13,  1366,  1366,   250,   277,   321,    30,
    -365,  -365,    55,   359,    62,   389,   397,   449,   452,  -365,
     223,    79,  -365,  -365,  2111,  2111,   506,    45,   509,   563,
     566,   620,  -365,  -365,  -365,  -365,  -365,   623,  -365,   148,
      87,  -365,  2059,   454,   142,   151,  -365,   511,   154,  -365,
     568,   182,  -365,   625,  -365,  -365,  1843,  -365,   289,  -365,
    -365,  -365,  -365,  -365,  -365,  -365,  -365,  -365,  -365,  -365,
    -365,  -365,  -365,  -365,  -365,  -365,  -365,  -365,  -365,    59,
     160,   181,    63,  -365,  -365,  -365,   202,  -365,  -365,  -365,
    -365,  -365,  -365,  -365,    83,  -365,  1667,   116,  1667,   116,
    -365,   910,  -365,   967,  -365,  1024,  -365,  -365,  1942,   197,
    -365,   853,    23,    26,  -365,   164,  -365,   209,    12,    39,
    -365,   120,  -365,   218,    15,   214,   238,   252,  -365,   256,
    -365,  1969,  -365,  -365,   221,  -365,  -365,  -365,  -365,   301,
    -365,  -365,   248,  -365,   148,   148,  1755,  1799,  1309,   279,
     286,  2111,  1309,  1843,  -365,   247,  -365,  -365,  -365,  -365,
    -365,  -365,  -365,  1799,   176,   259,   317,  1309,  1309,  1309,
    -365,  1421,  1081,  1667,  -365,  -365,    72,   354,   119,  1915,
    -365,  -365,  -365,  1942,  1942,  -365,  2111,  2111,  1711,   215,
    -365,  -365,  -365,   262,  -365,  -365,    28,  2085,    23,  1195,
     209,   275,  -365,    39,  1309,   218,   305,   305,   306,   309,
    1969,   313,   315,  1969,  -365,  -365,  -365,  -365,  -365,  -365,
    -365,   169,   272,   330,  -365,  -365,  -365,  -365,  2137,  -365,
    1799,   272,  -365,   190,  -365,  1309,   328,   331,  -365,  -365,
    -365,  -365,  -365,  -365,  1138,   121,  -365,  1462,  1667,  -365,
    1503,  1667,   299,   300,  1755,  1799,    51,  1309,  1843,  -365,
     203,  -365,   336,   352,    29,   351,  -365,  -365,  -365,  -365,
    -365,  -365,  1887,  -365,  -365,   353,  -365,  -365,   356,  -365,
    -365,   305,   305,  -365,   305,   305,  -365,  2085,  -365,   272,
    -365,  -365,  1309,  1309,  -365,  -365,  1544,  -365,   152,  -365,
     156,   358,   360,   177,   210,   364,  -365,  1799,   212,  -365,
      16,  1309,  1309,   352,  1309,   195,  -365,  -365,  -365,   220,
    -365,   361,   366,  -365,  -365,  1969,  1969,  -365,  -365,  -365,
    -365,  1887,  -365,  -365,  -365,  -365,  1585,  -365,  1626,  1309,
    1309,  2085,  -365,  -365,   240,  -365,  -365,  -365,   362,   374,
     375,  -365,   195,  1252,   361,  -365,  -365,  1969,  1969,  1969,
    1969,   370,  -365,  -365,   376,   377,  1887,  -365,  -365,  -365,
    -365,  -365,  -365,   391,  -365,  -365,  -365,  -365,  -365,  -365,
     246,  -365,  -365
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -365,  -365,   -27,  -365,   204,   281,  -143,   207,   -50,  -226,
     371,  -365,  -365,   294,  -365,  -365,   302,  -365,  -365,   291,
    -365,  -365,   293,  -365,  -365,   236,   -37,     0,  -116,  -149,
     -36,  -365,   251,    99,  -365,   -35,  -235,  -365,   -49,  -365,
    -340,  -365,    41,  -159,  -237,  -210,  -166,   232,  -364,  -344,
      32,   100,    91,  -168,   266,   -58,  -365,    -2,  -112
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -40
static const yytype_int16 yytable[] =
{
      39,   225,   188,   127,   312,   225,   291,   246,   225,   248,
     234,   225,   309,   310,   151,   245,   263,   401,   201,    60,
      61,   271,   212,   182,   127,   127,   189,   326,    39,   198,
     200,   117,   183,   120,   182,   123,   213,   431,   327,   183,
     326,   420,   183,    41,   183,   353,   171,   173,   356,   175,
     189,   166,    42,    46,   264,   183,   337,   181,    62,   225,
     167,   225,   326,   225,    63,    47,   289,   183,    48,   225,
     184,   436,   450,   190,   329,    49,   127,   240,   441,   361,
     312,   184,   168,   312,   315,   237,   316,   329,    43,    44,
     241,   169,    52,   289,   216,   217,   243,   190,   245,   317,
     245,   292,   343,    64,    65,   347,    50,   301,   221,   329,
      62,   244,   155,   222,   373,    51,   197,   303,    57,   156,
     157,   158,   159,   160,   188,   265,   233,   253,   254,   312,
     247,   161,   249,   319,   162,   365,   402,    53,    54,   151,
      58,   151,   163,   272,   154,   155,   320,   273,   366,   280,
      52,   283,   156,   157,   158,   159,   160,   218,   188,   368,
     425,   426,   370,   219,   161,   226,   405,   162,   293,   312,
     407,   312,   299,   220,   227,   163,    62,   229,   258,   406,
     127,   127,   199,   408,   359,   267,   238,   307,   307,   307,
     357,   268,   444,   445,   446,   447,   268,   127,   411,   304,
     334,   258,   225,   273,   268,   231,   385,   239,   338,   374,
     188,   183,   378,   360,   151,   242,   151,   273,   269,   345,
     261,   298,   127,   300,   348,   286,   379,   274,   202,   203,
     380,   341,   -18,   412,   276,   415,    51,   273,   -18,   273,
     314,   349,   350,   421,   -18,   195,   196,   422,   -18,   -18,
     206,   207,   208,   258,   258,   307,   324,   325,   277,   -22,
     290,   414,   127,   437,   127,   -22,   188,   273,   334,   452,
     302,   -22,   335,   422,   336,   -22,   -22,   376,   202,   203,
     258,   214,   215,   258,   392,   305,    -5,   346,   127,   127,
     151,   151,    -5,   151,   151,   235,   207,   208,    -5,   273,
     395,   396,    -5,    -5,   176,   177,   388,   288,   207,   208,
     278,   279,   307,   403,   281,   282,   397,   398,    62,   399,
     400,   341,   334,   306,   207,   208,   351,   392,   377,   352,
     -10,   417,   418,   354,   419,   355,   -10,   294,   295,   151,
     358,   127,   -10,   392,   296,   297,   -10,   -10,   427,   428,
     429,   430,   322,   323,   362,   363,   371,   372,   381,   434,
     435,   382,   384,   393,   392,   388,   394,   318,    -9,   409,
     423,   410,   438,   443,    -9,   341,   413,   385,   392,   151,
      -9,   151,   244,   155,    -9,    -9,   439,   440,   448,   449,
     156,   157,   158,   159,   160,   258,   258,   422,    -7,    69,
     388,   451,   161,    68,    -7,   162,    -8,   224,   287,   236,
      -7,   230,    -8,   163,    -7,    -7,   232,   262,    -8,   228,
     270,   416,    -8,    -8,   424,   383,   375,   258,   258,   258,
     258,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    70,    -6,    71,
     275,   -11,    27,     0,    -6,    72,     0,   -11,    73,     0,
      -6,    74,    75,   -11,    -6,    -6,     0,   -11,   -11,     0,
       0,     0,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,    70,   -12,    71,     0,    -4,     0,
       0,   -12,    72,     0,    -4,   115,     0,   -12,    74,    75,
      -4,   -12,   -12,     0,    -4,    -4,     0,     0,     0,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,    70,   -13,    71,     0,   -14,     0,     0,   -13,    72,
       0,   -14,   118,     0,   -13,    74,    75,   -14,   -13,   -13,
       0,   -14,   -14,     0,     0,     0,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,    70,   -15,
      71,     0,   -39,     0,     0,   -15,    72,     0,   -39,   121,
       0,   -15,    74,    75,   -39,   -15,   -15,     0,   -39,   -39,
       0,     0,     0,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,    70,     0,    71,     0,     0,
       0,     0,     0,    72,     0,     0,   170,     0,     0,    74,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,    70,     0,    71,     0,     0,     0,     0,     0,
      72,     0,     0,   172,     0,     0,    74,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,    70,
       0,    71,     0,     0,     0,     0,     0,    72,     0,     0,
     174,     0,     0,    74,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,    70,     0,    71,     0,
       0,     0,     0,     0,    72,     0,     0,   179,     0,     0,
      74,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,    70,     0,    71,     0,     0,     0,     0,
       0,    72,     0,     0,   250,     0,     0,    74,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
      70,     0,    71,     0,     0,     0,     0,     0,    72,     0,
       0,   251,     0,     0,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,    70,     0,    71,
       0,     0,     0,     0,     0,    72,     0,     0,   252,     0,
       0,    74,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,    70,     0,    71,     0,     0,     0,
       0,     0,    72,     0,     0,   313,     0,     0,    74,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,    70,     0,    71,     0,     0,     0,     0,     0,    72,
       0,     0,   364,     0,     0,    74,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,    70,     0,
      71,     0,     0,     0,     0,   344,    72,     0,     0,     0,
       0,     0,    74,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   129,   130,   131,   132,    80,    81,   133,
     134,   135,   136,   137,   138,   139,    89,   140,   141,    92,
      93,     0,   142,   143,   144,   145,   146,   147,   148,   102,
     103,   104,   105,   106,   211,    70,     0,    71,     0,     0,
       0,     0,   442,    72,     0,     0,     0,     0,     0,    74,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     129,   130,   131,   132,    80,    81,   133,   134,   135,   136,
     137,   138,   139,    89,   140,   141,    92,    93,     0,   142,
     143,   144,   145,   146,   147,   148,   102,   103,   104,   105,
     106,   211,    70,     0,    71,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,    74,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   129,   130,   131,
     132,    80,    81,   133,   134,   135,   136,   137,   138,   139,
      89,   140,   141,    92,    93,     0,   142,   143,   144,   145,
     146,   147,   148,   102,   103,   104,   105,   106,   211,    70,
       0,    71,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   129,   130,   131,   132,    80,    81,
     133,   134,   135,   136,   137,   138,   139,    89,   140,   141,
      92,    93,     0,   142,   143,   144,   145,   146,   147,   148,
     102,   103,   104,   105,   106,   211,   128,     0,     0,     0,
       0,     0,     0,     0,     0,   311,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   129,
     130,   131,   132,    80,    81,   133,   134,   135,   136,   137,
     138,   139,    89,   140,   141,    92,    93,   128,   142,   143,
     144,   145,   146,   147,   148,     0,   367,     0,   105,   149,
     150,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     129,   130,   131,   132,    80,    81,   133,   134,   135,   136,
     137,   138,   139,    89,   140,   141,    92,    93,   128,   142,
     143,   144,   145,   146,   147,   148,     0,   369,     0,   105,
     149,   150,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   129,   130,   131,   132,    80,    81,   133,   134,   135,
     136,   137,   138,   139,    89,   140,   141,    92,    93,   128,
     142,   143,   144,   145,   146,   147,   148,     0,   404,     0,
     105,   149,   150,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   129,   130,   131,   132,    80,    81,   133,   134,
     135,   136,   137,   138,   139,    89,   140,   141,    92,    93,
     128,   142,   143,   144,   145,   146,   147,   148,     0,   432,
       0,   105,   149,   150,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   129,   130,   131,   132,    80,    81,   133,
     134,   135,   136,   137,   138,   139,    89,   140,   141,    92,
      93,   128,   142,   143,   144,   145,   146,   147,   148,     0,
     433,     0,   105,   149,   150,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   129,   130,   131,   132,    80,    81,
     133,   134,   135,   136,   137,   138,   139,    89,   140,   141,
      92,    93,   128,   142,   143,   144,   145,   146,   147,   148,
       0,     0,     0,   105,   149,   150,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   129,   130,   131,   132,    80,
      81,   133,   134,   135,   136,   137,   138,   139,    89,   140,
     141,    92,    93,     0,   142,   143,   144,   145,   146,   147,
     148,     0,   326,     0,   105,   149,   150,   183,     0,     0,
       0,     0,     0,   327,     0,     0,     0,     0,     0,   125,
       2,     3,     4,     0,     0,     7,     8,     9,    10,    11,
      12,    13,     0,    15,    16,     0,     0,     0,   328,    21,
      22,    23,    24,    25,    26,     0,   182,     0,     0,   329,
      27,   183,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     2,     3,     4,     0,     0,     7,
       8,     9,    10,    11,    12,    13,     0,    15,    16,     0,
       0,     0,     0,    21,    22,    23,    24,    25,    26,     0,
     189,     0,     0,   184,    27,   183,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,     2,     3,
       4,     0,     0,     7,     8,     9,    10,    11,    12,    13,
       0,    15,    16,     0,     0,     0,     0,    21,    22,    23,
      24,    25,    26,     0,   189,     0,     0,   190,    27,   183,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     2,     3,     4,     0,     0,     7,     8,     9,
      10,    11,    12,    13,     0,    15,    16,     0,     0,     0,
       0,    21,    22,    23,    24,    25,    26,     0,   385,     0,
       0,   190,    27,   183,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     2,     3,     4,     0,
       0,     7,     8,     9,    10,    11,    12,    13,   321,    15,
      16,     0,     0,     0,     0,    21,    22,    23,    24,    25,
      26,     0,     0,   244,   155,   387,    27,     0,     0,     0,
       0,   156,   157,   158,   159,   160,   255,     0,     0,     0,
       0,     0,     0,   161,     0,     0,   162,     0,     0,     0,
       1,     2,     3,     4,   163,     0,     7,     8,     9,    10,
      11,    12,    13,   284,    15,    16,     0,     0,   256,   257,
      21,    22,    23,    24,    25,    26,     0,     1,     2,     3,
       4,    27,     0,     7,     8,     9,    10,    11,    12,    13,
       0,    15,    16,     0,     0,   256,   257,    21,    22,    23,
      24,    25,    26,     0,   124,     0,     0,     0,    27,   125,
       2,     3,     4,     0,     0,     7,     8,     9,    10,    11,
      12,    13,     0,    15,    16,     0,     0,     0,   126,    21,
      22,    23,    24,    25,    26,     0,     0,     0,     0,     0,
      27,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,   125,     2,     3,
       4,     0,    27,     7,     8,     9,    10,    11,    12,    13,
       0,    15,    16,     0,     0,     0,   223,    21,    22,    23,
      24,    25,    26,   339,     2,     3,     4,     0,    27,     7,
       8,     9,    10,    11,    12,    13,     0,    15,    16,     0,
       0,     0,     0,    21,    22,    23,    24,    25,    26,     1,
       2,     3,     4,   340,    27,     7,     8,     9,    10,    11,
      12,    13,     0,    15,    16,     0,     0,     0,     0,    21,
      22,    23,    24,    25,    26,   125,     2,     3,     4,     0,
      27,     7,     8,     9,    10,    11,    12,    13,     0,    15,
      16,     0,     0,     0,     0,    21,    22,    23,    24,    25,
      26,     0,     0,     0,     0,     0,    27
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-365))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
       0,   113,    60,    39,   241,   117,   216,   166,   120,   168,
     126,   123,   238,   239,    41,   164,   182,   357,    67,    19,
      20,   189,    72,    11,    60,    61,    11,    11,    28,    64,
      65,    33,    16,    35,    11,    37,    23,   401,    22,    16,
      11,   385,    16,    13,    16,   280,    48,    49,   283,    51,
      11,    13,    22,    13,    28,    16,    28,    59,    13,   171,
      22,   173,    11,   175,    19,    13,   209,    16,    13,   181,
      58,   411,   436,    58,    58,    13,   112,    14,   422,   305,
     317,    58,    13,   320,   243,    26,    14,    58,    58,    59,
      27,    22,    13,   236,    94,    95,    13,    58,   247,    27,
     249,   217,   268,    58,    59,   273,    13,   223,    21,    58,
      13,    28,    29,    26,   324,    13,    19,   233,    13,    36,
      37,    38,    39,    40,   182,   183,   126,   176,   177,   366,
     167,    48,   169,    14,    51,    14,   362,    58,    59,   166,
      13,   168,    59,    23,    28,    29,    27,    27,    27,   198,
      13,   200,    36,    37,    38,    39,    40,     9,   216,   318,
     395,   396,   321,    15,    48,    23,    14,    51,   218,   406,
      14,   408,   222,    25,    23,    59,    13,    23,   178,    27,
     216,   217,    19,    27,   300,    21,    26,   237,   238,   239,
      21,    27,   427,   428,   429,   430,    27,   233,    21,    23,
     258,   201,   314,    27,    27,    23,    11,    26,   266,   325,
     268,    16,   328,    23,   241,    13,   243,    27,     9,   269,
      23,   221,   258,   223,   274,     4,    23,     9,     7,     8,
      27,   267,     9,    23,    20,    23,    13,    27,    15,    27,
     242,   276,   277,    23,    21,    58,    59,    27,    25,    26,
       6,     7,     8,   253,   254,   305,   256,   257,    20,     9,
      12,   377,   298,    23,   300,    15,   324,    27,   326,    23,
      23,    21,    57,    27,    12,    25,    26,   327,     7,     8,
     280,    74,    75,   283,   342,    26,     9,    12,   324,   325,
     317,   318,    15,   320,   321,     6,     7,     8,    21,    27,
     349,   350,    25,    26,    53,    54,   342,     6,     7,     8,
      58,    59,   362,   363,    58,    59,   351,   352,    13,   354,
     355,   357,   380,     6,     7,     8,    20,   385,   328,    20,
       9,   381,   382,    20,   384,    20,    15,    58,    59,   366,
      10,   377,    21,   401,    58,    59,    25,    26,   397,   398,
     399,   400,   253,   254,    26,    24,    57,    57,    22,   409,
     410,     9,    11,    10,   422,   401,    10,    13,     9,    11,
       9,    11,    10,   423,    15,   411,    12,    11,   436,   406,
      21,   408,    28,    29,    25,    26,    12,    12,    12,    12,
      36,    37,    38,    39,    40,   395,   396,    27,     9,    28,
     436,    10,    48,     0,    15,    51,     9,   113,   204,   128,
      21,   120,    15,    59,    25,    26,   123,   181,    21,   117,
     188,   380,    25,    26,   392,   334,   326,   427,   428,   429,
     430,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,     3,     9,     5,
     194,     9,    59,    -1,    15,    11,    -1,    15,    14,    -1,
      21,    17,    18,    21,    25,    26,    -1,    25,    26,    -1,
      -1,    -1,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,     3,     9,     5,    -1,     9,    -1,
      -1,    15,    11,    -1,    15,    14,    -1,    21,    17,    18,
      21,    25,    26,    -1,    25,    26,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,     3,     9,     5,    -1,     9,    -1,    -1,    15,    11,
      -1,    15,    14,    -1,    21,    17,    18,    21,    25,    26,
      -1,    25,    26,    -1,    -1,    -1,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,     3,     9,
       5,    -1,     9,    -1,    -1,    15,    11,    -1,    15,    14,
      -1,    21,    17,    18,    21,    25,    26,    -1,    25,    26,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,     3,    -1,     5,    -1,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    14,    -1,    -1,    17,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,     3,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    14,    -1,    -1,    17,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,     3,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      14,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,     3,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    14,    -1,    -1,
      17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,     3,    -1,     5,    -1,    -1,    -1,    -1,
      -1,    11,    -1,    -1,    14,    -1,    -1,    17,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
       3,    -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    14,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,     3,    -1,     5,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    14,    -1,
      -1,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,     3,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    14,    -1,    -1,    17,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,     3,    -1,     5,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    14,    -1,    -1,    17,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,     3,    -1,
       5,    -1,    -1,    -1,    -1,    10,    11,    -1,    -1,    -1,
      -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,     3,    -1,     5,    -1,    -1,
      -1,    -1,    10,    11,    -1,    -1,    -1,    -1,    -1,    17,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,     3,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    -1,    -1,    -1,    17,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,     3,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,     5,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    14,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     5,    47,    48,
      49,    50,    51,    52,    53,    -1,    14,    -1,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     5,    47,
      48,    49,    50,    51,    52,    53,    -1,    14,    -1,    57,
      58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     5,
      47,    48,    49,    50,    51,    52,    53,    -1,    14,    -1,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       5,    47,    48,    49,    50,    51,    52,    53,    -1,    14,
      -1,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     5,    47,    48,    49,    50,    51,    52,    53,    -1,
      14,    -1,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     5,    47,    48,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    -1,    11,    -1,    57,    58,    59,    16,    -1,    -1,
      -1,    -1,    -1,    22,    -1,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    -1,    -1,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    -1,    -1,    -1,    47,    48,
      49,    50,    51,    52,    53,    -1,    11,    -1,    -1,    58,
      59,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    -1,    -1,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    -1,
      11,    -1,    -1,    58,    59,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,    30,
      31,    -1,    -1,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    -1,    11,    -1,    -1,    58,    59,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    28,    29,    30,    31,    -1,    -1,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    -1,    11,    -1,
      -1,    58,    59,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    -1,
      -1,    34,    35,    36,    37,    38,    39,    40,    13,    42,
      43,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    -1,    -1,    28,    29,    58,    59,    -1,    -1,    -1,
      -1,    36,    37,    38,    39,    40,    14,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    51,    -1,    -1,    -1,
      28,    29,    30,    31,    59,    -1,    34,    35,    36,    37,
      38,    39,    40,    14,    42,    43,    -1,    -1,    46,    47,
      48,    49,    50,    51,    52,    53,    -1,    28,    29,    30,
      31,    59,    -1,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    -1,    -1,    46,    47,    48,    49,    50,
      51,    52,    53,    -1,    23,    -1,    -1,    -1,    59,    28,
      29,    30,    31,    -1,    -1,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    -1,    -1,    -1,    47,    48,
      49,    50,    51,    52,    53,    -1,    -1,    -1,    -1,    -1,
      59,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    28,    29,    30,
      31,    -1,    59,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    -1,    -1,    -1,    47,    48,    49,    50,
      51,    52,    53,    28,    29,    30,    31,    -1,    59,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    28,
      29,    30,    31,    58,    59,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    28,    29,    30,    31,    -1,
      59,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    -1,    -1,    -1,    -1,    -1,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    59,    62,    71,
      72,    73,    75,    76,    78,    79,    81,    82,    84,    88,
      91,    13,    22,    58,    59,    98,    13,    13,    13,    13,
      13,    13,    13,    58,    59,    92,    93,    13,    13,    85,
      88,    88,    13,    19,    58,    59,    95,    96,     0,    71,
       3,     5,    11,    14,    17,    18,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    63,    68,
      69,    74,    88,   118,   119,    14,    77,   118,    14,    80,
     118,    14,    83,   118,    23,    28,    47,    91,     5,    28,
      29,    30,    31,    34,    35,    36,    37,    38,    39,    40,
      42,    43,    47,    48,    49,    50,    51,    52,    53,    58,
      59,    63,   104,   105,    28,    29,    36,    37,    38,    39,
      40,    48,    51,    59,    87,    90,    13,    22,    13,    22,
      14,   118,    14,   118,    14,   118,    93,    93,    99,    14,
      86,   118,    11,    16,    58,   106,   107,   108,   116,    11,
      58,    89,   114,   115,   116,    58,    59,    19,    96,    19,
      96,    99,     7,     8,    64,    65,     6,     7,     8,    66,
      67,    59,    69,    23,    68,    68,    88,    88,     9,    15,
      25,    21,    26,    47,    74,   119,    23,    23,    77,    23,
      80,    23,    83,    88,    89,     6,    66,    26,    26,    26,
      14,    27,    13,    13,    28,    90,   104,    87,   104,    87,
      14,    14,    14,    99,    99,    14,    46,    47,    88,    94,
     100,    23,    86,   107,    28,   116,   117,    21,    27,     9,
     108,   114,    23,    27,     9,   115,    20,    20,    58,    59,
      99,    58,    59,    99,    14,    97,     4,    65,     6,    67,
      12,   106,    89,    69,    58,    59,    58,    59,    88,    69,
      88,    89,    23,    89,    23,    26,     6,    69,    70,    70,
      70,    14,   105,    14,   118,   104,    14,    27,    13,    14,
      27,    13,    94,    94,    88,    88,    11,    22,    47,    58,
     102,   103,   112,   113,   116,    57,    12,    28,   116,    28,
      58,    91,   101,   107,    10,    69,    12,   114,    69,    96,
      96,    20,    20,    97,    20,    20,    97,    21,    10,    89,
      23,    70,    26,    24,    14,    14,    27,    14,   104,    14,
     104,    57,    57,   106,    89,   112,    69,    88,    89,    23,
      27,    22,     9,   113,    11,    11,    28,    58,    91,   109,
     110,   111,   116,    10,    10,    99,    99,    96,    96,    96,
      96,   101,    70,    69,    14,    14,    27,    14,    27,    11,
      11,    21,    23,    12,    89,    23,   103,    69,    69,    69,
     110,    23,    27,     9,   111,    97,    97,    99,    99,    99,
      99,   109,    14,    14,    69,    69,   101,    23,    10,    12,
      12,   110,    10,    69,    97,    97,    97,    97,    12,    12,
     109,    10,    23
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
#line 972 "ctf-parser.y"
    {
			if (set_parent_node((yyvsp[(1) - (1)].n), &ctf_scanner_get_ast(scanner)->root))
				reparent_error(scanner, "error reparenting to root");
		}
    break;

  case 3:
/* Line 1787 of yacc.c  */
#line 977 "ctf-parser.y"
    {
			if (set_parent_node((yyvsp[(2) - (2)].n), &ctf_scanner_get_ast(scanner)->root))
				reparent_error(scanner, "error reparenting to root");
		}
    break;

  case 4:
/* Line 1787 of yacc.c  */
#line 985 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 5:
/* Line 1787 of yacc.c  */
#line 987 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 6:
/* Line 1787 of yacc.c  */
#line 989 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 7:
/* Line 1787 of yacc.c  */
#line 991 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 8:
/* Line 1787 of yacc.c  */
#line 993 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 9:
/* Line 1787 of yacc.c  */
#line 995 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 10:
/* Line 1787 of yacc.c  */
#line 997 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 11:
/* Line 1787 of yacc.c  */
#line 999 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 12:
/* Line 1787 of yacc.c  */
#line 1001 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 13:
/* Line 1787 of yacc.c  */
#line 1003 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 14:
/* Line 1787 of yacc.c  */
#line 1005 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 15:
/* Line 1787 of yacc.c  */
#line 1007 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 16:
/* Line 1787 of yacc.c  */
#line 1009 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 17:
/* Line 1787 of yacc.c  */
#line 1011 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 18:
/* Line 1787 of yacc.c  */
#line 1013 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 19:
/* Line 1787 of yacc.c  */
#line 1015 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 20:
/* Line 1787 of yacc.c  */
#line 1017 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 21:
/* Line 1787 of yacc.c  */
#line 1019 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 22:
/* Line 1787 of yacc.c  */
#line 1021 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 23:
/* Line 1787 of yacc.c  */
#line 1023 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 24:
/* Line 1787 of yacc.c  */
#line 1025 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 25:
/* Line 1787 of yacc.c  */
#line 1027 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 26:
/* Line 1787 of yacc.c  */
#line 1029 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 27:
/* Line 1787 of yacc.c  */
#line 1031 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 28:
/* Line 1787 of yacc.c  */
#line 1033 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 29:
/* Line 1787 of yacc.c  */
#line 1035 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;		}
    break;

  case 30:
/* Line 1787 of yacc.c  */
#line 1042 "ctf-parser.y"
    {	(yyval.gs) = (yyvsp[(1) - (1)].gs);					}
    break;

  case 31:
/* Line 1787 of yacc.c  */
#line 1044 "ctf-parser.y"
    {	(yyval.gs) = gc_string_append(scanner, (yyvsp[(1) - (2)].gs), (yyvsp[(2) - (2)].gs));		}
    break;

  case 32:
/* Line 1787 of yacc.c  */
#line 1049 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;					}
    break;

  case 33:
/* Line 1787 of yacc.c  */
#line 1051 "ctf-parser.y"
    {
			reparent_error(scanner, "escape sequences not supported yet");
		}
    break;

  case 34:
/* Line 1787 of yacc.c  */
#line 1060 "ctf-parser.y"
    {	(yyval.gs) = (yyvsp[(1) - (1)].gs);					}
    break;

  case 35:
/* Line 1787 of yacc.c  */
#line 1062 "ctf-parser.y"
    {	(yyval.gs) = gc_string_append(scanner, (yyvsp[(1) - (2)].gs), (yyvsp[(2) - (2)].gs));		}
    break;

  case 36:
/* Line 1787 of yacc.c  */
#line 1067 "ctf-parser.y"
    {	(yyval.gs) = yylval.gs;					}
    break;

  case 37:
/* Line 1787 of yacc.c  */
#line 1069 "ctf-parser.y"
    {
			reparent_error(scanner, "escape sequences not supported yet");
		}
    break;

  case 38:
/* Line 1787 of yacc.c  */
#line 1078 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
		}
    break;

  case 39:
/* Line 1787 of yacc.c  */
#line 1084 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
		}
    break;

  case 40:
/* Line 1787 of yacc.c  */
#line 1090 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
		}
    break;

  case 41:
/* Line 1787 of yacc.c  */
#line 1096 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			sscanf(yylval.gs->s, "%" PRIu64,
			       &(yyval.n)->u.unary_expression.u.unsigned_constant);
		}
    break;

  case 42:
/* Line 1787 of yacc.c  */
#line 1103 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			sscanf(yylval.gs->s, "0%" PRIo64,
			       &(yyval.n)->u.unary_expression.u.unsigned_constant);
		}
    break;

  case 43:
/* Line 1787 of yacc.c  */
#line 1110 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
			sscanf(yylval.gs->s, "0x%" PRIx64,
			       &(yyval.n)->u.unary_expression.u.unsigned_constant);
		}
    break;

  case 44:
/* Line 1787 of yacc.c  */
#line 1117 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = "";
		}
    break;

  case 45:
/* Line 1787 of yacc.c  */
#line 1123 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = (yyvsp[(2) - (3)].gs)->s;
		}
    break;

  case 46:
/* Line 1787 of yacc.c  */
#line 1129 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = (yyvsp[(2) - (3)].gs)->s;
		}
    break;

  case 47:
/* Line 1787 of yacc.c  */
#line 1135 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_NESTED;
			(yyval.n)->u.unary_expression.u.nested_exp = (yyvsp[(2) - (3)].n);
		}
    break;

  case 48:
/* Line 1787 of yacc.c  */
#line 1141 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_SBRAC;
			(yyval.n)->u.unary_expression.u.sbrac_exp = (yyvsp[(3) - (4)].n);
			bt_list_splice(&((yyvsp[(1) - (4)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 49:
/* Line 1787 of yacc.c  */
#line 1149 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
			(yyval.n)->u.unary_expression.link = UNARY_DOTLINK;
			bt_list_splice(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 50:
/* Line 1787 of yacc.c  */
#line 1158 "ctf-parser.y"
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
#line 1167 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_UNARY_EXPRESSION);
			(yyval.n)->u.unary_expression.type = UNARY_STRING;
			(yyval.n)->u.unary_expression.u.string = yylval.gs->s;
			(yyval.n)->u.unary_expression.link = UNARY_ARROWLINK;
			bt_list_splice(&((yyvsp[(1) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			bt_list_add_tail(&((yyval.n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 52:
/* Line 1787 of yacc.c  */
#line 1176 "ctf-parser.y"
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
#line 1188 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);				}
    break;

  case 54:
/* Line 1787 of yacc.c  */
#line 1190 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(2) - (2)].n);				}
    break;

  case 55:
/* Line 1787 of yacc.c  */
#line 1192 "ctf-parser.y"
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

  case 56:
/* Line 1787 of yacc.c  */
#line 1211 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
			(yyvsp[(3) - (3)].n)->u.unary_expression.link = UNARY_DOTDOTDOT;
		}
    break;

  case 57:
/* Line 1787 of yacc.c  */
#line 1217 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);		}
    break;

  case 58:
/* Line 1787 of yacc.c  */
#line 1224 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (2)].n);	}
    break;

  case 59:
/* Line 1787 of yacc.c  */
#line 1226 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 60:
/* Line 1787 of yacc.c  */
#line 1228 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 61:
/* Line 1787 of yacc.c  */
#line 1230 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 62:
/* Line 1787 of yacc.c  */
#line 1232 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 63:
/* Line 1787 of yacc.c  */
#line 1234 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 64:
/* Line 1787 of yacc.c  */
#line 1236 "ctf-parser.y"
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

  case 65:
/* Line 1787 of yacc.c  */
#line 1247 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 66:
/* Line 1787 of yacc.c  */
#line 1257 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(1) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 67:
/* Line 1787 of yacc.c  */
#line 1267 "ctf-parser.y"
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

  case 68:
/* Line 1787 of yacc.c  */
#line 1288 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_EVENT);
		}
    break;

  case 69:
/* Line 1787 of yacc.c  */
#line 1292 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_EVENT);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "event_declaration");
		}
    break;

  case 70:
/* Line 1787 of yacc.c  */
#line 1301 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 71:
/* Line 1787 of yacc.c  */
#line 1306 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 72:
/* Line 1787 of yacc.c  */
#line 1312 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STREAM);
		}
    break;

  case 73:
/* Line 1787 of yacc.c  */
#line 1316 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STREAM);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "stream_declaration");
		}
    break;

  case 74:
/* Line 1787 of yacc.c  */
#line 1325 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 75:
/* Line 1787 of yacc.c  */
#line 1330 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 76:
/* Line 1787 of yacc.c  */
#line 1335 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENV);
		}
    break;

  case 77:
/* Line 1787 of yacc.c  */
#line 1339 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENV);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "env declaration");
		}
    break;

  case 78:
/* Line 1787 of yacc.c  */
#line 1348 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 79:
/* Line 1787 of yacc.c  */
#line 1353 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 80:
/* Line 1787 of yacc.c  */
#line 1358 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TRACE);
		}
    break;

  case 81:
/* Line 1787 of yacc.c  */
#line 1362 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TRACE);
			if (set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
    break;

  case 82:
/* Line 1787 of yacc.c  */
#line 1371 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 83:
/* Line 1787 of yacc.c  */
#line 1376 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 84:
/* Line 1787 of yacc.c  */
#line 1381 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_CLOCK);
		}
    break;

  case 85:
/* Line 1787 of yacc.c  */
#line 1385 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_CLOCK);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "trace_declaration");
		}
    break;

  case 86:
/* Line 1787 of yacc.c  */
#line 1394 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 87:
/* Line 1787 of yacc.c  */
#line 1399 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 88:
/* Line 1787 of yacc.c  */
#line 1404 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 89:
/* Line 1787 of yacc.c  */
#line 1413 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[(1) - (1)].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 90:
/* Line 1787 of yacc.c  */
#line 1421 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 91:
/* Line 1787 of yacc.c  */
#line 1430 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (2)].n);
			bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 92:
/* Line 1787 of yacc.c  */
#line 1438 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 93:
/* Line 1787 of yacc.c  */
#line 1447 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[(1) - (1)].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 94:
/* Line 1787 of yacc.c  */
#line 1455 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 95:
/* Line 1787 of yacc.c  */
#line 1464 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (2)].n);
			bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 96:
/* Line 1787 of yacc.c  */
#line 1472 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 97:
/* Line 1787 of yacc.c  */
#line 1474 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 98:
/* Line 1787 of yacc.c  */
#line 1482 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_CHAR;
		}
    break;

  case 99:
/* Line 1787 of yacc.c  */
#line 1487 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SHORT;
		}
    break;

  case 100:
/* Line 1787 of yacc.c  */
#line 1492 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INT;
		}
    break;

  case 101:
/* Line 1787 of yacc.c  */
#line 1497 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_LONG;
		}
    break;

  case 102:
/* Line 1787 of yacc.c  */
#line 1502 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SIGNED;
		}
    break;

  case 103:
/* Line 1787 of yacc.c  */
#line 1507 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_UNSIGNED;
		}
    break;

  case 104:
/* Line 1787 of yacc.c  */
#line 1512 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_BOOL;
		}
    break;

  case 105:
/* Line 1787 of yacc.c  */
#line 1517 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ID_TYPE;
			(yyval.n)->u.type_specifier.id_type = yylval.gs->s;
		}
    break;

  case 106:
/* Line 1787 of yacc.c  */
#line 1523 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
		}
    break;

  case 107:
/* Line 1787 of yacc.c  */
#line 1529 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "integer reparent error");
		}
    break;

  case 108:
/* Line 1787 of yacc.c  */
#line 1540 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_VOID;
		}
    break;

  case 109:
/* Line 1787 of yacc.c  */
#line 1545 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_CHAR;
		}
    break;

  case 110:
/* Line 1787 of yacc.c  */
#line 1550 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SHORT;
		}
    break;

  case 111:
/* Line 1787 of yacc.c  */
#line 1555 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INT;
		}
    break;

  case 112:
/* Line 1787 of yacc.c  */
#line 1560 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_LONG;
		}
    break;

  case 113:
/* Line 1787 of yacc.c  */
#line 1565 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOAT;
		}
    break;

  case 114:
/* Line 1787 of yacc.c  */
#line 1570 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_DOUBLE;
		}
    break;

  case 115:
/* Line 1787 of yacc.c  */
#line 1575 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_SIGNED;
		}
    break;

  case 116:
/* Line 1787 of yacc.c  */
#line 1580 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_UNSIGNED;
		}
    break;

  case 117:
/* Line 1787 of yacc.c  */
#line 1585 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_BOOL;
		}
    break;

  case 118:
/* Line 1787 of yacc.c  */
#line 1590 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_COMPLEX;
		}
    break;

  case 119:
/* Line 1787 of yacc.c  */
#line 1595 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_IMAGINARY;
		}
    break;

  case 120:
/* Line 1787 of yacc.c  */
#line 1600 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ID_TYPE;
			(yyval.n)->u.type_specifier.id_type = yylval.gs->s;
		}
    break;

  case 121:
/* Line 1787 of yacc.c  */
#line 1606 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_FLOATING_POINT);
		}
    break;

  case 122:
/* Line 1787 of yacc.c  */
#line 1612 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_FLOATING_POINT);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "floating point reparent error");
		}
    break;

  case 123:
/* Line 1787 of yacc.c  */
#line 1620 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
		}
    break;

  case 124:
/* Line 1787 of yacc.c  */
#line 1626 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_INTEGER;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_INTEGER);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "integer reparent error");
		}
    break;

  case 125:
/* Line 1787 of yacc.c  */
#line 1634 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
		}
    break;

  case 126:
/* Line 1787 of yacc.c  */
#line 1640 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
		}
    break;

  case 127:
/* Line 1787 of yacc.c  */
#line 1646 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRING;
			(yyval.n)->u.type_specifier.node = make_node(scanner, NODE_STRING);
			if (set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)->u.type_specifier.node))
				reparent_error(scanner, "string reparent error");
		}
    break;

  case 128:
/* Line 1787 of yacc.c  */
#line 1654 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_ENUM;
			(yyval.n)->u.type_specifier.node = (yyvsp[(2) - (2)].n);
		}
    break;

  case 129:
/* Line 1787 of yacc.c  */
#line 1660 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_VARIANT;
			(yyval.n)->u.type_specifier.node = (yyvsp[(2) - (2)].n);
		}
    break;

  case 130:
/* Line 1787 of yacc.c  */
#line 1666 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER);
			(yyval.n)->u.type_specifier.type = TYPESPEC_STRUCT;
			(yyval.n)->u.type_specifier.node = (yyvsp[(2) - (2)].n);
		}
    break;

  case 131:
/* Line 1787 of yacc.c  */
#line 1675 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			if ((yyvsp[(2) - (3)].n) && set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 132:
/* Line 1787 of yacc.c  */
#line 1682 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 133:
/* Line 1787 of yacc.c  */
#line 1690 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 134:
/* Line 1787 of yacc.c  */
#line 1698 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 0;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 135:
/* Line 1787 of yacc.c  */
#line 1704 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 0;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 136:
/* Line 1787 of yacc.c  */
#line 1710 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			bt_list_add_tail(&((yyvsp[(6) - (7)].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[(2) - (7)].n) && set_parent_node((yyvsp[(2) - (7)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 137:
/* Line 1787 of yacc.c  */
#line 1718 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (8)].gs)->s;
			bt_list_add_tail(&((yyvsp[(7) - (8)].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[(3) - (8)].n) && set_parent_node((yyvsp[(3) - (8)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 138:
/* Line 1787 of yacc.c  */
#line 1727 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_STRUCT);
			(yyval.n)->u._struct.has_body = 1;
			(yyval.n)->u._struct.name = (yyvsp[(1) - (8)].gs)->s;
			bt_list_add_tail(&((yyvsp[(7) - (8)].n))->siblings, &(yyval.n)->u._struct.min_align);
			if ((yyvsp[(3) - (8)].n) && set_parent_node((yyvsp[(3) - (8)].n), (yyval.n)))
				reparent_error(scanner, "struct reparent error");
		}
    break;

  case 139:
/* Line 1787 of yacc.c  */
#line 1739 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 140:
/* Line 1787 of yacc.c  */
#line 1744 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 141:
/* Line 1787 of yacc.c  */
#line 1749 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			if ((yyvsp[(2) - (3)].n) && set_parent_node((yyvsp[(2) - (3)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 142:
/* Line 1787 of yacc.c  */
#line 1756 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.choice = (yyvsp[(2) - (6)].gs)->s;
			if ((yyvsp[(5) - (6)].n) && set_parent_node((yyvsp[(5) - (6)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 143:
/* Line 1787 of yacc.c  */
#line 1764 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.choice = (yyvsp[(2) - (6)].gs)->s;
			if ((yyvsp[(5) - (6)].n) && set_parent_node((yyvsp[(5) - (6)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 144:
/* Line 1787 of yacc.c  */
#line 1772 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 145:
/* Line 1787 of yacc.c  */
#line 1780 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 146:
/* Line 1787 of yacc.c  */
#line 1789 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 147:
/* Line 1787 of yacc.c  */
#line 1796 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 148:
/* Line 1787 of yacc.c  */
#line 1805 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 149:
/* Line 1787 of yacc.c  */
#line 1812 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			if ((yyvsp[(3) - (4)].n) && set_parent_node((yyvsp[(3) - (4)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 150:
/* Line 1787 of yacc.c  */
#line 1820 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 151:
/* Line 1787 of yacc.c  */
#line 1829 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 152:
/* Line 1787 of yacc.c  */
#line 1836 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 1;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (7)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (7)].gs)->s;
			if ((yyvsp[(6) - (7)].n) && set_parent_node((yyvsp[(6) - (7)].n), (yyval.n)))
				reparent_error(scanner, "variant reparent error");
		}
    break;

  case 153:
/* Line 1787 of yacc.c  */
#line 1845 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_VARIANT);
			(yyval.n)->u.variant.has_body = 0;
			(yyval.n)->u.variant.name = (yyvsp[(1) - (4)].gs)->s;
			(yyval.n)->u.variant.choice = (yyvsp[(3) - (4)].gs)->s;
		}
    break;

  case 154:
/* Line 1787 of yacc.c  */
#line 1855 "ctf-parser.y"
    {	push_scope(scanner);	}
    break;

  case 155:
/* Line 1787 of yacc.c  */
#line 1860 "ctf-parser.y"
    {	pop_scope(scanner);	}
    break;

  case 156:
/* Line 1787 of yacc.c  */
#line 1865 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			_bt_list_splice_tail(&((yyvsp[(2) - (3)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 157:
/* Line 1787 of yacc.c  */
#line 1871 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			((yyval.n))->u._enum.container_type = (yyvsp[(2) - (5)].n);
			_bt_list_splice_tail(&((yyvsp[(4) - (5)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 158:
/* Line 1787 of yacc.c  */
#line 1878 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (4)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 159:
/* Line 1787 of yacc.c  */
#line 1885 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (6)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (6)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (6)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 160:
/* Line 1787 of yacc.c  */
#line 1893 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (4)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 161:
/* Line 1787 of yacc.c  */
#line 1900 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (6)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (6)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (6)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 162:
/* Line 1787 of yacc.c  */
#line 1908 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			_bt_list_splice_tail(&((yyvsp[(2) - (4)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 163:
/* Line 1787 of yacc.c  */
#line 1914 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			((yyval.n))->u._enum.container_type = (yyvsp[(2) - (6)].n);
			_bt_list_splice_tail(&((yyvsp[(4) - (6)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 164:
/* Line 1787 of yacc.c  */
#line 1921 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (5)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (5)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 165:
/* Line 1787 of yacc.c  */
#line 1928 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (7)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (7)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (7)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 166:
/* Line 1787 of yacc.c  */
#line 1936 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 0;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 167:
/* Line 1787 of yacc.c  */
#line 1942 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (5)].gs)->s;
			_bt_list_splice_tail(&((yyvsp[(3) - (5)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 168:
/* Line 1787 of yacc.c  */
#line 1949 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 1;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (7)].gs)->s;
			((yyval.n))->u._enum.container_type = (yyvsp[(3) - (7)].n);
			_bt_list_splice_tail(&((yyvsp[(5) - (7)].n))->tmp_head, &((yyval.n))->u._enum.enumerator_list);
		}
    break;

  case 169:
/* Line 1787 of yacc.c  */
#line 1957 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUM);
			(yyval.n)->u._enum.has_body = 0;
			(yyval.n)->u._enum.enum_id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 170:
/* Line 1787 of yacc.c  */
#line 1966 "ctf-parser.y"
    {	(yyval.n) = NULL;	}
    break;

  case 171:
/* Line 1787 of yacc.c  */
#line 1968 "ctf-parser.y"
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

  case 172:
/* Line 1787 of yacc.c  */
#line 1981 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (3)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_STRUCT_OR_VARIANT_DECLARATION);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (3)].n))->tmp_head, &((yyval.n))->u.struct_or_variant_declaration.type_declarators);
		}
    break;

  case 173:
/* Line 1787 of yacc.c  */
#line 1991 "ctf-parser.y"
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

  case 174:
/* Line 1787 of yacc.c  */
#line 2002 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 175:
/* Line 1787 of yacc.c  */
#line 2012 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (4)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 176:
/* Line 1787 of yacc.c  */
#line 2022 "ctf-parser.y"
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

  case 177:
/* Line 1787 of yacc.c  */
#line 2043 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 178:
/* Line 1787 of yacc.c  */
#line 2052 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			node = (yyvsp[(1) - (1)].n);
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 179:
/* Line 1787 of yacc.c  */
#line 2060 "ctf-parser.y"
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

  case 180:
/* Line 1787 of yacc.c  */
#line 2071 "ctf-parser.y"
    {
			struct ctf_node *node;

			(yyval.n) = (yyvsp[(1) - (2)].n);
			node = make_node(scanner, NODE_TYPE_SPECIFIER);
			node->u.type_specifier.type = TYPESPEC_CONST;
			bt_list_add_tail(&node->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 181:
/* Line 1787 of yacc.c  */
#line 2080 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (2)].n);
			bt_list_add_tail(&((yyvsp[(2) - (2)].n))->siblings, &((yyval.n))->u.type_specifier_list.head);
		}
    break;

  case 182:
/* Line 1787 of yacc.c  */
#line 2085 "ctf-parser.y"
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

  case 183:
/* Line 1787 of yacc.c  */
#line 2099 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 184:
/* Line 1787 of yacc.c  */
#line 2101 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 185:
/* Line 1787 of yacc.c  */
#line 2109 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 186:
/* Line 1787 of yacc.c  */
#line 2111 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(2) - (2)].n);	}
    break;

  case 187:
/* Line 1787 of yacc.c  */
#line 2113 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			if (set_parent_node((yyvsp[(3) - (3)].n), (yyvsp[(1) - (3)].n)))
				reparent_error(scanner, "struct_or_variant_declarator");
		}
    break;

  case 188:
/* Line 1787 of yacc.c  */
#line 2122 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 189:
/* Line 1787 of yacc.c  */
#line 2124 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 190:
/* Line 1787 of yacc.c  */
#line 2132 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 191:
/* Line 1787 of yacc.c  */
#line 2137 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 192:
/* Line 1787 of yacc.c  */
#line 2142 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 193:
/* Line 1787 of yacc.c  */
#line 2147 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = "";
		}
    break;

  case 194:
/* Line 1787 of yacc.c  */
#line 2152 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(2) - (3)].gs)->s;
		}
    break;

  case 195:
/* Line 1787 of yacc.c  */
#line 2157 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (3)].gs)->s;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 196:
/* Line 1787 of yacc.c  */
#line 2163 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (3)].gs)->s;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 197:
/* Line 1787 of yacc.c  */
#line 2169 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(1) - (3)].gs)->s;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 198:
/* Line 1787 of yacc.c  */
#line 2175 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = "";
			bt_list_splice(&((yyvsp[(4) - (4)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 199:
/* Line 1787 of yacc.c  */
#line 2181 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_ENUMERATOR);
			(yyval.n)->u.enumerator.id = (yyvsp[(2) - (5)].gs)->s;
			bt_list_splice(&((yyvsp[(5) - (5)].n))->tmp_head, &((yyval.n))->u.enumerator.values);
		}
    break;

  case 200:
/* Line 1787 of yacc.c  */
#line 2190 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 201:
/* Line 1787 of yacc.c  */
#line 2192 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 202:
/* Line 1787 of yacc.c  */
#line 2200 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 203:
/* Line 1787 of yacc.c  */
#line 2202 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 204:
/* Line 1787 of yacc.c  */
#line 2210 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
                        (yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			/* id is NULL */
		}
    break;

  case 205:
/* Line 1787 of yacc.c  */
#line 2216 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 206:
/* Line 1787 of yacc.c  */
#line 2222 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 207:
/* Line 1787 of yacc.c  */
#line 2228 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 208:
/* Line 1787 of yacc.c  */
#line 2236 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (3)].n);
			(yyval.n)->u.type_declarator.u.nested.abstract_array = 1;
		}
    break;

  case 209:
/* Line 1787 of yacc.c  */
#line 2246 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 210:
/* Line 1787 of yacc.c  */
#line 2248 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(3) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 211:
/* Line 1787 of yacc.c  */
#line 2256 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 212:
/* Line 1787 of yacc.c  */
#line 2258 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 213:
/* Line 1787 of yacc.c  */
#line 2266 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
                        (yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			/* id is NULL */
		}
    break;

  case 214:
/* Line 1787 of yacc.c  */
#line 2272 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 215:
/* Line 1787 of yacc.c  */
#line 2278 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 216:
/* Line 1787 of yacc.c  */
#line 2286 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (3)].n);
			(yyval.n)->u.type_declarator.u.nested.abstract_array = 1;
		}
    break;

  case 217:
/* Line 1787 of yacc.c  */
#line 2296 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 218:
/* Line 1787 of yacc.c  */
#line 2298 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 219:
/* Line 1787 of yacc.c  */
#line 2306 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 220:
/* Line 1787 of yacc.c  */
#line 2312 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 221:
/* Line 1787 of yacc.c  */
#line 2318 "ctf-parser.y"
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
#line 2329 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (1)].n);	}
    break;

  case 223:
/* Line 1787 of yacc.c  */
#line 2331 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(2) - (2)].n);
			bt_list_splice(&((yyvsp[(1) - (2)].n))->tmp_head, &((yyval.n))->u.type_declarator.pointers);
		}
    break;

  case 224:
/* Line 1787 of yacc.c  */
#line 2339 "ctf-parser.y"
    {
			add_type(scanner, (yyvsp[(1) - (1)].gs));
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_ID;
			(yyval.n)->u.type_declarator.u.id = (yyvsp[(1) - (1)].gs)->s;
		}
    break;

  case 225:
/* Line 1787 of yacc.c  */
#line 2346 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(2) - (3)].n);
		}
    break;

  case 226:
/* Line 1787 of yacc.c  */
#line 2352 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_TYPE_DECLARATOR);
			(yyval.n)->u.type_declarator.type = TYPEDEC_NESTED;
			(yyval.n)->u.type_declarator.u.nested.type_declarator = (yyvsp[(1) - (4)].n);
			BT_INIT_LIST_HEAD(&((yyval.n))->u.type_declarator.u.nested.length);
			_bt_list_splice_tail(&((yyvsp[(3) - (4)].n))->tmp_head, &((yyval.n))->u.type_declarator.u.nested.length);
		}
    break;

  case 227:
/* Line 1787 of yacc.c  */
#line 2363 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
		}
    break;

  case 228:
/* Line 1787 of yacc.c  */
#line 2367 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
			bt_list_splice(&((yyvsp[(2) - (2)].n))->tmp_head, &((yyval.n))->tmp_head);
		}
    break;

  case 229:
/* Line 1787 of yacc.c  */
#line 2372 "ctf-parser.y"
    {
			(yyval.n) = make_node(scanner, NODE_POINTER);
			(yyval.n)->u.pointer.const_qualifier = 1;
			bt_list_splice(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->tmp_head);
		}
    break;

  case 232:
/* Line 1787 of yacc.c  */
#line 2389 "ctf-parser.y"
    {	(yyval.n) = (yyvsp[(1) - (2)].n);	}
    break;

  case 233:
/* Line 1787 of yacc.c  */
#line 2391 "ctf-parser.y"
    {
			(yyval.n) = (yyvsp[(1) - (3)].n);
			bt_list_add_tail(&((yyvsp[(2) - (3)].n))->siblings, &((yyval.n))->tmp_head);
		}
    break;

  case 234:
/* Line 1787 of yacc.c  */
#line 2399 "ctf-parser.y"
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

  case 235:
/* Line 1787 of yacc.c  */
#line 2411 "ctf-parser.y"
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

  case 236:
/* Line 1787 of yacc.c  */
#line 2423 "ctf-parser.y"
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

  case 237:
/* Line 1787 of yacc.c  */
#line 2434 "ctf-parser.y"
    {
			struct ctf_node *list;

			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			(yyval.n)->u._typedef.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(2) - (3)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 238:
/* Line 1787 of yacc.c  */
#line 2444 "ctf-parser.y"
    {
			struct ctf_node *list;

			list = make_node(scanner, NODE_TYPE_SPECIFIER_LIST);
			_bt_list_splice_tail(&((yyvsp[(1) - (3)].n))->u.type_specifier_list.head, &list->u.type_specifier_list.head);
			(yyval.n) = make_node(scanner, NODE_TYPEDEF);
			((yyval.n))->u.struct_or_variant_declaration.type_specifier_list = list;
			_bt_list_splice_tail(&((yyvsp[(3) - (3)].n))->tmp_head, &((yyval.n))->u._typedef.type_declarators);
		}
    break;

  case 239:
/* Line 1787 of yacc.c  */
#line 2454 "ctf-parser.y"
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
#line 5427 "ctf-parser.c"
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


