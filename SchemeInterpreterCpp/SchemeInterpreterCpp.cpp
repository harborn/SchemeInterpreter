// Scheme Interpreter in 90 lines of C++ (not counting lines after the first 90).
// Inspired by Peter Norvig's Lis.py.

// Made by Anthony C. Hay in 2010. See http://howtowriteaprogram.blogspot.co.uk/
// This is free and unencumbered public domain software, see http://unlicense.org/
// This code is known to have faults. E.g. it leaks memory. Use at your own risk.

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <cassert>

//cell eval(cell x, environment * env);

// return given mumber as a string
std::string str(long n) { std::ostringstream os; os << n; return os.str(); }
//std::string str(int i) { std::ostringstream os; os << i; return os.str(); }
//std::string str(double d) { std::ostringstream os; os << d; return os.str(); }

// return true iff given character is '0'..'9'
bool isdig(char c) { return isdigit(static_cast<unsigned char>(c)) != 0; }


////////////////////// cell

enum cell_type { Symbol, Number, List, Proc, Lambda };

struct environment; // forward declaration; cell and environment reference each other



// a variant that can hold any kind of lisp value
struct cell {
	typedef cell(*proc_type)(const std::vector<cell> &);
	typedef std::vector<cell>::const_iterator iter;
	typedef std::map<std::string, cell> map;
	cell_type type;
	std::string val;
	std::vector<cell> list;
	proc_type proc;
	environment * env;
	cell(cell_type type = Symbol) : type(type), env(0) {}
	cell(cell_type type, const std::string & val) : type(type), val(val), env(0) {}
	cell(proc_type proc) : type(Proc), proc(proc), env(0) {}
};

typedef std::vector<cell> cells;
typedef cells::const_iterator cellit;

const cell false_sym(Symbol, "#f");
const cell true_sym(Symbol, "#t"); // anything that isn't false_sym is true
const cell nil(Symbol, "nil");
const cell define_sym(Symbol, "define");
const cell lambda_sym(Symbol, "lambda");
const cell if_sym(Symbol, "if");
const cell begin_sym(Symbol, "begin");
const cell quote_sym(Symbol, "quote");
const cell set_sym(Symbol, "set!");
const cell cond_sym(Symbol, "cond");

////////////////////// environment

// a dictionary that (a) associates symbols with cells, and
// (b) can chain to an "outer" dictionary
struct environment {
	environment(environment * outer = 0) : outer_(outer) {}

	environment(const cells & parms, const cells & args, environment * outer)
		: outer_(outer)
	{
		cellit a = args.begin();
		for (cellit p = parms.begin(); p != parms.end(); ++p)
			env_[p->val] = *a++;
	}

	// map a variable name onto a cell
	typedef std::map<std::string, cell> map;

	// return a reference to the innermost environment where 'var' appears
	map & find(const std::string & var)
	{
		if (env_.find(var) != env_.end())
			return env_; // the symbol exists in this environment
		if (outer_)
			return outer_->find(var); // attempt to find the symbol in some "outer" env
		std::cout << "unbound symbol '" << var << "'\n";
		exit(1);
	}

	// return a reference to the cell associated with the given symbol 'var'
	cell & operator[] (const std::string & var)
	{
		return env_[var];
	}

private:
	map env_; // inner symbol->cell mapping
	environment * outer_; // next adjacent outer env, or 0 if there are no further environments
};

environment global_env;

////////////////////// built-in primitive procedures

cell proc_add(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i) n += atol(i->val.c_str());
	return cell(Number, str(n));
}

cell proc_sub(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i) n -= atol(i->val.c_str());
	return cell(Number, str(n));
}

cell proc_mul(const cells & c)
{
	long n(1);
	for (cellit i = c.begin(); i != c.end(); ++i) n *= atol(i->val.c_str());
	return cell(Number, str(n));
}

cell proc_div(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i) n /= atol(i->val.c_str());
	return cell(Number, str(n));
}

cell proc_sqrt(const cells & c)
{
	double d(atof(c[0].val.c_str()));
	std::ostringstream os; os << sqrt(d);
	return cell(Number, os.str());
}

cell proc_greater(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i)
		if (n <= atol(i->val.c_str()))
			return false_sym;
	return true_sym;
}

cell proc_less(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i)
		if (n >= atol(i->val.c_str()))
			return false_sym;
	return true_sym;
}

cell proc_less_equal(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i)
		if (n > atol(i->val.c_str()))
			return false_sym;
	return true_sym;
}

cell proc_equal(const cells & c)
{
	long n(atol(c[0].val.c_str()));
	for (cellit i = c.begin() + 1; i != c.end(); ++i)
		if (n != atol(i->val.c_str()))
			return false_sym;
	return true_sym;
}

cell proc_is(const cells & c)
{
	return cell();
}

cell proc_display(const cells & c)
{
	return cell();
}

cell proc_length(const cells & c) { return cell(Number, str(c[0].list.size())); }
cell proc_nullp(const cells & c)  {
	return c[0].val == "" && c[0].list.empty() ? true_sym : false_sym;
}
cell proc_car(const cells & c)    { return c[0].list[0]; }

cell proc_cdr(const cells & c)
{
	if (c[0].list.size() < 2)
		return nil;
	cell result(c[0]);
	result.list.erase(result.list.begin());
	return result;
}

cell proc_append(const cells & c)
{
	cell result(List);
	result.list = c[0].list;
	for (cellit i = c[1].list.begin(); i != c[1].list.end(); ++i) result.list.push_back(*i);
	return result;
}

cell proc_cons(const cells & c)
{
	cell result(List);
	result.list.push_back(c[0]);
	for (cellit i = c[1].list.begin(); i != c[1].list.end(); ++i) result.list.push_back(*i);
	return result;
}

cell proc_list(const cells & c)
{
	cell result(List); result.list = c;
	return result;
}

cell proc_listp(const cells & c)
{
	return c[0].type == List ? true_sym : false_sym;
}

cell proc_pairp(const cells & c)
{
	return (c[0].type == List && c[0].list.size() > 0) ? true_sym : false_sym;
}

cell proc_apply(const cells & c)
{
	return cell();
}

cell proc_callcc(const cells & c)
{
	return cell();
}

cell eval(cell x, environment * env);

cell proc_eval(const cells & c)
{
	return eval(c[0], &global_env);
}

cell proc_emptyp(const cells & c)
{
	return (c[0].type == List || c[0].type == Symbol) && c[0].list.empty() ? true_sym : false_sym;
}

// define the bare minimum set of primintives necessary to pass the unit tests
void add_globals(environment & env)
{
	env["#f"] = false_sym;					env["#t"] = true_sym;
	env["append"] = cell(&proc_append);		env["car"] = cell(&proc_car);
	env["cdr"] = cell(&proc_cdr);			env["cons"] = cell(&proc_cons);
	env["length"] = cell(&proc_length);		env["list"] = cell(&proc_list);
	env["null?"] = cell(&proc_nullp);		env["+"] = cell(&proc_add);
	env["-"] = cell(&proc_sub);				env["*"] = cell(&proc_mul);
	env["/"] = cell(&proc_div);				env[">"] = cell(&proc_greater);
	env["<"] = cell(&proc_less);			env["<="] = cell(&proc_less_equal);
	env["="] = cell(&proc_equal);			env["nil"] = nil;
	env["equal?"] = cell(&proc_equal);		env["eq?"] = cell(&proc_is);
	env["display"] = cell(&proc_display);	env["sqrt"] = cell(&proc_sqrt);
	env[""] = cell();						env["list?"] = cell(&proc_listp);
	env["pair?"] = cell(&proc_pairp);		env["apply"] = cell(&proc_apply);
	env["callcc"] = cell(&proc_callcc);		env["eval"] = cell(&proc_eval);
	env["empty?"] = cell(&proc_emptyp);
}


////////////////////// eval

cell eval(cell x, environment * env)
{
	if (x.type == Symbol)
		return env->find(x.val)[x.val];
	if (x.type == Number)
		return x;
	if (x.list.empty())
		return nil;
	if (x.list[0].type == Symbol) {
		if (x.list[0].val == "quote")       // (quote exp)
			return x.list[1];
		if (x.list[0].val == "if")          // (if test conseq [alt])
			return eval(eval(x.list[1], env).val == "#f" ? (x.list.size() < 4 ? nil : x.list[3]) : x.list[2], env);
		if (x.list[0].val == "set!")        // (set! var exp)
			return env->find(x.list[1].val)[x.list[1].val] = eval(x.list[2], env);
		if (x.list[0].val == "define")      // (define var exp)
			return (*env)[x.list[1].val] = eval(x.list[2], env);
		if (x.list[0].val == "lambda") {    // (lambda (var*) exp)
			x.type = Lambda;
			// keep a reference to the environment that exists now (when the
			// lambda is being defined) because that's the outer environment
			// we'll need to use when the lambda is executed
			x.env = env;
			return x;
		}
		if (x.list[0].val == "begin") {     // (begin exp*)
			for (size_t i = 1; i < x.list.size() - 1; ++i)
				eval(x.list[i], env);
			return eval(x.list[x.list.size() - 1], env);
		}
	}
	// (proc exp*)
	cell proc(eval(x.list[0], env));
	cells exps;
	for (cell::iter exp = x.list.begin() + 1; exp != x.list.end(); ++exp)
		exps.push_back(eval(*exp, env));
	if (proc.type == Lambda) {
		// Create an environment for the execution of this lambda function
		// where the outer environment is the one that existed* at the time
		// the lambda was defined and the new inner associations are the
		// parameter names with the given arguments.
		// *Although the environmet existed at the time the lambda was defined
		// it wasn't necessarily complete - it may have subsequently had
		// more symbols defined in that environment.
		return eval(/*body*/proc.list[2], new environment(/*parms*/proc.list[1].list, /*args*/exps, proc.env));
	}
	else if (proc.type == Proc)
		return proc.proc(exps);

	std::cout << "not a function\n";
	exit(1);
}

// print cell
void printCell(const cell &c)
{
	if (c.type == Symbol || c.type == Number) {
		std::cout << c.val << " ";
	}
	else if (c.type == List) {
		std::cout << "(";
		for (cellit it = c.list.begin(); it != c.list.end(); it++) {
			printCell(*it);
		}
		std::cout << ")";
	}
}

cell expandCond(const cell &c)
{
	cell ret(List);
	ret.list.push_back(if_sym);
	for (cellit it = c.list[1].list.begin(); it != c.list[1].list.end(); it++)
		ret.list.push_back(*it);
	if (c.list.size() > 1) {
		if (c.list[2].list[0].val == "else") {
			for (cellit it = c.list[2].list.begin() + 1; it != c.list[2].list.end(); it++)
				ret.list.push_back(*it);
		}
		else {
			cell cond(List);
			cond.list.push_back(cond_sym);
			for (cellit it = c.list.begin() + 2; it != c.list.end(); it++)
				cond.list.push_back(*it);
			ret.list.push_back(expandCond(cond));
		}
	}
	return ret;
}

cell expandCase(const cell &c)
{
	return cell();
}

// expand define 
cell expand(const cell &c)
{
	if (c.type != List) {
		return cell(c);
	}
	else if (c.list[0].val == "quote") {
		assert(c.list.size() == 2);
		return cell(c);
	}
	else if (c.list[0].val == "if") {
		cell cc(c);
		if (cc.list.size() == 3)		// (if t c) => (if t c None)
			cc.list.push_back(cell());
		assert(cc.list.size() == 4);
		cell ret(List);
		for (cellit it = cc.list.begin(); it != cc.list.end(); it++) {
			ret.list.push_back(expand(*it));
		}
		return ret;
	}
	else if (c.list[0].val == "set!") {			// (set! non-var exp) => Error
		assert(c.list.size() == 3);
		assert(c.list[1].type == Symbol);
		cell ret(List);
		ret.list.push_back(set_sym);
		ret.list.push_back(c.list[1]);
		ret.list.push_back(expand(c.list[2]));
		return ret;
	}
	else if (c.list[0].val == "define") {
		assert(c.list.size() >= 3);
		cell v(c.list[1]);
		cell body(c.list[2]);
		if (v.type == List) {
			cell cc(List);
			cc.list.push_back(define_sym); // define
			cc.list.push_back(v.list[0]); // name
			cell ccc(List); // lambda args body
			ccc.list.push_back(lambda_sym);
			cell args(List);
			for (cellit it = v.list.begin() + 1; it != v.list.end(); it++) {
				args.list.push_back(*it);
			}
			ccc.list.push_back(args);
			for (cellit it = c.list.begin() + 2; it != c.list.end(); it++) {
				ccc.list.push_back(*it);
			}
			cc.list.push_back(ccc);
			return expand(cc);
		}
		else {
			cell cc = expand(c.list[2]);
			cell ret(List);
			ret.list.push_back(define_sym);
			ret.list.push_back(c.list[1]);
			ret.list.push_back(cc);
			return ret;
		}
	}
	else if (c.list[0].val == "begin") {
		if (c.list.size() == 1) return cell(); // (begin) => None
		cell ret(List);
		ret.list.push_back(begin_sym);
		for (cellit it = c.list.begin() + 1; it != c.list.end(); it++) {
			ret.list.push_back(expand(*it));
		}
		return ret;
	}
	else if (c.list[0].val == "lambda") {	// (lambda (x) e) or (lambda (x) e1 e2)
		assert(c.list.size() >= 3);
		cell ret(List);
		ret.list.push_back(c.list[0]);
		ret.list.push_back(c.list[1]);
		cell exp(List);
		if (c.list.size() > 3) {			// => (lambda (x) (begin e1 e2))
			exp.list.push_back(begin_sym);
			for (cellit it = c.list.begin() + 2; it != c.list.end(); it++) {
				exp.list.push_back(*it);
			}
		}
		else {								// => (lambda (x) e)
			for (cellit it = c.list[2].list.begin(); it != c.list[2].list.end(); it++) {
				exp.list.push_back(*it);
			}
		}
		ret.list.push_back(expand(exp));
		return ret;
	}
	else if (c.list[0].val == "cond") { // (cond <clause1> <clause2> ...)
		cell ret = expandCond(c);
		return ret;
	}
	else {
		cell ret(List);
		for (cellit it = c.list.begin(); it != c.list.end(); it++) {
			ret.list.push_back(expand(*it));
		}
		return ret;
	}
}

////////////////////// parse, read and user interaction

// convert given string to list of tokens
std::list<std::string> tokenize(const std::string & str)
{
	std::list<std::string> tokens;
	const char * s = str.c_str();
	while (*s) {
		while (*s == ' ')
			++s;
		if (*s == '(' || *s == ')')
			tokens.push_back(*s++ == '(' ? "(" : ")");
		else {
			const char * t = s;
			while (*t && *t != ' ' && *t != '(' && *t != ')')
				++t;
			tokens.push_back(std::string(s, t));
			s = t;
		}
	}
	return tokens;
}

// numbers become Numbers; every other token is a Symbol
cell atom(const std::string & token)
{
	if (isdig(token[0]) || (token[0] == '-' && isdig(token[1])))
		return cell(Number, token);
	return cell(Symbol, token);
}

// return the Lisp expression in the given tokens
cell read_from(std::list<std::string> & tokens)
{
	const std::string token(tokens.front());
	tokens.pop_front();
	if (token == "(") {
		cell c(List);
		while (tokens.front() != ")")
			c.list.push_back(read_from(tokens));
		tokens.pop_front();
		return c;
	}
	else if (token[0] == '`' || token[0] == '\'') {
		cell c(List);
		c.list.push_back(quote_sym);
		if (token.length() == 1)
			c.list.push_back(read_from(tokens));
		else
			c.list.push_back(atom(token.substr(1, token.length() - 1)));
		return c;
	}
	else
		return atom(token);
}

// tokenize the input string,
// construct cell by tokens
// and expand the cell
cell read(const std::string &s)
{
	std::list<std::string> tokens(tokenize(s));
	cell c = read_from(tokens);
	cell ret(expand(c));
	//printCell(ret);
	//std::cout << std::endl;
	return ret;
}

// convert given cell to a Lisp-readable string
std::string to_string(const cell & exp)
{
	if (exp.type == List) {
		std::string s("(");
		for (cell::iter e = exp.list.begin(); e != exp.list.end(); ++e)
			s += to_string(*e) + ' ';
		if (s[s.size() - 1] == ' ')
			s.erase(s.size() - 1);
		return s + ')';
	}
	else if (exp.type == Lambda)
		return "<Lambda>";
	else if (exp.type == Proc)
		return "<Proc>";
	return exp.val;
}

// the default read-eval-print-loop
void repl(const std::string & prompt, environment * env)
{
	for (;;) {
		std::cout << prompt;
		std::string line; std::getline(std::cin, line);
		std::cout << to_string(eval(read(line), env)) << '\n';
	}
}

int main(void)
{
	add_globals(global_env);
	repl("SICPP> ", &global_env);

	std::string exps[] = {
		"(empty? 3)",
		"(car '((1) (2 3) (4) (5 6)))",
		"(cdr '((1) (2 3) (4) (5 6 (7 8 (9)))))",
		"(cond ((> 3 3) `greater) ((< 3 3) `less) (else `equal))",
		//"(define (empty? lst) (if (= (length lst) 0) (list? `()) (list? 3)))",
		"(define (my-sum lst) (cond ((empty? lst) 0) ((list? (car lst)) (+ (my-sum (car lst)) (my-sum (cdr lst)))) (else (+ (car lst) (my-sum (cdr lst))))))",
		"(my-sum '((1) (2 3) (4) (5 6)))",
		"(my-sum '((1) (2 3) (4) (5 6 (7 8 (9)))))",
		"(sqrt 10)",
		"(define ((account bal) amt) (set! bal (+ bal amt)) bal)",
		"(define a1(account 100))",
		"(a1 0)",
		"(a1 10)",
		"(a1 10)",
		"(define abs (lambda (n) ((if (> n 0) + -) 0 n)))",
		"(list (abs -3) (abs 0) (abs 3))",
		"(define (newton guess function derivative epsilon) (define guess2(- guess (/ (function guess) (derivative guess)))) (if (< (abs (- guess guess2)) epsilon) guess2 (newton guess2 function derivative epsilon)))",
		"(define (square-root a) (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 1e-8))",
		"(> (square-root 200.) 14.14213)",
		"(< (square-root 200.) 14.14215)",
		"(= (square-root 200.) (sqrt 200.))",
		"(callcc (lambda (throw) (+ 5 (* 10 (throw 1)))))", // throw 1
		"(callcc (lambda (throw) (+ 5 (* 10 1))))", // throw 15
		"(callcc (lambda (throw) (+5 (*10 (callcc(lambda(escape) (*100 (escape 3))))))))", // throw 35
		"(callcc (lambda (throw) (+5 (*10 (callcc(lambda(escape) (*100 (throw 3))))))))", // throw 3
		"(callcc (lambda (throw) (+5 (*10 (callcc(lambda(escape) (*100 1)))))))", // throw 1005
		"(eval `(+ 3 4))",
		"(pair? 3)",
		"(pair? (cons 3 `()))",
		"(pair? (list 3 4))",
		"(pair? (cons `() `()))",
		"(pair? (list))",
		"(pair? (list 3))",
		"(pair? `())",
		"(define ex `(+ 3 4))",
		"(define x `())",
		"x",
		"(define x (quote ()))",
		"x",
		"(define nil `())",
		"nil",
		"(quote ())",
		"``a",
		"(define l (list 3 4))",
		"(define lst (cons 3 nil))",
		"lst",
		"(list? 3)",
		"(list? l)",
		"(list? lst)",
		"(list? nil)",
		"(null? `())",
		"(null? 3)",
		"(define str `hello)",
		"(define str 'hello)",
		"(define str (quote hello))",
		"(define x 10)",
		"(define (foo) (define x 4) x)",
		"(define (bar) (set! x 4) x)",
		"(define y (list 1 2 3 4 5 6))",
		"(define greeted nil)",

		//"(define (greet name) (set!greeted(cons name greeted)) (string - append \"Hello, \" name))",
		//"(define (add x y) (+ x y))",
		"(define add (lambda (x y) (+ x y)))",
		"(define add2 add)",
		//"(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))",
		"(define (double4 x) (define (double2 x) (+ x x)) (+ (double2 x) (double2 x)))",
		//"(define double4 (lambda (x) (+ (double2 x) (double2 x))))",
		//"(define double4 (lambda (x) (begin (define double2 (lambda (x) (+ x x))) (+ (double2 x) (double2 x)))))",
		//"(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
		"(define (fib n) (define (fib-kernal n1 n2 f1 f2) (if (< n1 n2) (fib-kernal (+ n1 1) n2 (+ f1 f2) f1) f2)) (fib-kernal 0 n 1 0))",
	};
	int en = sizeof(exps) / sizeof(exps[0]);

	for (int i = 0; i < en; i++) {
		std::cout << exps[i] << "\t => \t";
		std::cout << to_string(eval(read(exps[i]), &global_env)) << std::endl;
	}

	std::string evas[] = {
		"(foo)",
		"x",
		"(bar)",
		"x",
		"(add 3 4 5 6 7 8)",
		"(double4 10)",
		"(add2 3 4)",
		"(fib x)",
		//"(if (> 3 4) 0)",
		//"(if (> 3 4) 0 1)",
	};

	int vn = sizeof(evas) / sizeof(evas[0]);

	for (int i = 0; i < vn; i++) {
		std::cout << evas[i] << "\t => \t";
		std::cout << to_string(eval(read(evas[i]), &global_env)) << std::endl;
	}

	return 0;
}
