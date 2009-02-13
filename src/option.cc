/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "option.h"

namespace ledger {

namespace {
  typedef std::pair<expr_t::ptr_op_t, bool> op_bool_tuple;

  op_bool_tuple find_option(scope_t& scope, const string& name)
  {
    char buf[128];
    std::strcpy(buf, "opt_");
    char * p = &buf[4];
    foreach (char ch, name) {
      if (ch == '-')
	*p++ = '_';
      else
	*p++ = ch;
    }
    *p++ = '_';
    *p = '\0';

    if (expr_t::ptr_op_t op = scope.lookup(buf))
      return op_bool_tuple(op, true);

    *--p = '\0';

    return op_bool_tuple(scope.lookup(buf), false);
  }

  op_bool_tuple find_option(scope_t& scope, const char letter)
  {
    char buf[10];
    std::strcpy(buf, "opt_");
    buf[4] = letter;
    buf[5] = '_';
    buf[6] = '\0';

    if (expr_t::ptr_op_t op = scope.lookup(buf))
      return op_bool_tuple(op, true);

    buf[5] = '\0';

    return op_bool_tuple(scope.lookup(buf), false);
  }

  void process_option(const function_t& opt, scope_t& scope,
		      const char * arg, const string& name)
  {
    try {
      call_scope_t args(scope);
      if (arg)
	args.push_back(string_value(arg));

      opt(args);
    }
    catch (const std::exception& err) {
      if (name[0] == '-')
	add_error_context("While parsing option '" << name << "':");
	  
      else
	add_error_context("While parsing environent variable '"
			  << name << "':");
      throw;
    }
  }
}

void process_option(const string& name, scope_t& scope,
		    const char * arg, const string& varname)
{
  op_bool_tuple opt(find_option(scope, name));
  if (opt.first)
    process_option(opt.first->as_function(), scope, arg, varname);
}

void process_environment(const char ** envp, const string& tag,
			 scope_t& scope)
{
  const char * tag_p   = tag.c_str();
  std::size_t  tag_len = tag.length();

  for (const char ** p = envp; *p; p++) {
    if (! tag_p || std::strncmp(*p, tag_p, tag_len) == 0) {
      char   buf[128];
      char * r = buf;
      const char * q;
      for (q = *p + tag_len;
	   *q && *q != '=' && r - buf < 128;
	   q++)
	if (*q == '_')
	  *r++ = '-';
	else
	  *r++ = std::tolower(*q);
      *r = '\0';

      if (*q == '=') {
	try {
	  string value = string(*p, q - *p);
	  if (! value.empty())
	    process_option(string(buf), scope, q + 1, value);
	}
	catch (const std::exception& err) {
	  add_error_context("While parsing environment variable option '"
			    << *p << "':");
	  throw;
	}
      }
    }
  }
}

namespace {
  struct op_bool_char_tuple {
    expr_t::ptr_op_t op;
    bool truth;
    char ch;

    op_bool_char_tuple(expr_t::ptr_op_t _op, bool _truth, char _ch)
      : op(_op), truth(_truth), ch(_ch) {}
  };
}    

strings_list process_arguments(strings_list args, scope_t& scope)
{
  bool anywhere = true;

  strings_list remaining;

  for (strings_list::iterator i = args.begin();
       i != args.end();
       i++) {
    DEBUG("option.args", "Examining argument '" << *i << "'");

    if (! anywhere || (*i)[0] != '-') {
      DEBUG("option.args", "  adding to list of real args");
      remaining.push_back(*i);
      continue;
    }

    // --long-option or -s
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0') {
	DEBUG("option.args", "  it's a --, ending options processing");
	anywhere = false;
	continue;
      }

      DEBUG("option.args", "  it's an option string");

      string       opt_name;
      const char * name  = (*i).c_str() + 2;
      const char * value = NULL;

      if (const char * p = std::strchr(name, '=')) {
	opt_name = string(name, p - name);
	value = ++p;
	DEBUG("option.args", "  read option value from option: " << value);
      } else {
	opt_name = name;
      }

      op_bool_tuple opt(find_option(scope, opt_name));
      if (! opt.first)
	throw_(option_error, "illegal option --" << name);

      if (opt.second && value == NULL) {
	value = (*++i).c_str();
	DEBUG("option.args", "  read option value from arg: " << value);
	if (value == NULL)
	  throw_(option_error, "missing option argument for --" << name);
      }
      process_option(opt.first->as_function(), scope, value,
		     string("--") + name);
    }
    else if ((*i)[1] == '\0') {
      throw_(option_error, "illegal option -");
    }
    else {
      DEBUG("option.args", "  single-char option");

      std::list<op_bool_char_tuple> option_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	op_bool_tuple opt(find_option(scope, c));
	if (! opt.first)
	  throw_(option_error, "illegal option -" << c);

	option_queue.push_back(op_bool_char_tuple(opt.first, opt.second, c));
      }

      foreach (op_bool_char_tuple& o, option_queue) {
	const char * value = NULL;
	if (o.truth) {
	  value = (*++i).c_str();
	  DEBUG("option.args", "  read option value from arg: " << value);
	  if (value == NULL)
	    throw_(option_error,
		   "missing option argument for -" << o.ch);
	}
	process_option(o.op->as_function(), scope, value, string("-") + o.ch);
      }
    }
  }

  return remaining;
}

} // namespace ledger