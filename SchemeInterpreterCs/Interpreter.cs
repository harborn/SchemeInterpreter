using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;
using System.Threading.Tasks;

namespace SchemeInterpreterCs
{


    class Interpreter
    {

        static BigInteger Fib(BigInteger n1, BigInteger f1, BigInteger f2)
        {
            if (n1 == 0) return f2;
            else if (n1 == 1) return f1;
            else return Fib(n1 - 1, f1 + f2, f1);
        }


        static void Main(string[] args)
        {


            Interpreter ip = new Interpreter();

            string[] expr = {
                "(define q '())",
                "(define q2 '1)",
                "(define q3 'what)",
                "(define q4 '(ajfiodpsafe))",
                "q",
                "q2",
                "q3",
                "q4",

                "(define x 3)",
                "(define y (define x 4) x)",
                "x",
                "y",
                "(define testfoo (lambda (x y) (define add3 (lambda x (+ x 3))) (+ (add3 x) y)))",
                "(testfoo 3 4)",
                "(+ x y)",
                "((lambda (x y) (+ x y)) (+ 5 7) 6)",
                "(+ 3 4)",
                "(+ 3 4 (+ 3 4) (* 3 4) (/ 4 3))",
                "(define (add x y) (+ x y))",
                "(add 3 4)",
                "(define add (lambda (x y) (+ x y)))",
                "(define fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))",
                "(fib 5)",
                "(define (fib-kernel n1 n2 f1 f2) (if (< n1 n2) (fib-kernel (+ n1 1) n2 (+ f1 f2) f1) f2))",
                "(fib-kernel 0 10 1 0)",
                "(define (fib2 n) (define (fib-kernel n1 n2 f1 f2) (if (< n1 n2) (fib-kernel (+ n1 1) n2 (+ f1 f2) f1) f2)) (fib-kernel 0 n 1 0))",
                "(fib2 200)",
                //"(define add1 (lambda (x y) (+ x y)))",
                //"(define add2 (lambda (x y) (+ x y)))",
                //"(define add3 (lambda (x y) (+ x y)))",
                //"(define adds (list add1 add2 add3))",
                //"(define div1 /)",
                //"(define div2 /)",
                //"(define div3 /)",
                //"(define divs (list div1 div2 div3))",
                //"(define list1 '(1 2 3))",
                //"(define list2 '(4 5 6))",
                //"(define list3 '(7 8 9))",
                //"(define lists '(list1 list2 list3))",
                "(define list1 (list 1 2 3))",
                "(define list2 (list 4 5 6))",
                "(define list3 (list 7 8 9))",
                "(define lists (list list1 list2 list3))"
            };

            for (int i = 0; i < expr.Length; i++)
            {
                Console.Write(expr[i] + "  ------->  ");
                ip.EvalString(expr[i]);
                Console.WriteLine();
            }

            while (true)
            {
                Console.Write("Scheme>>> ");
                string line = Console.ReadLine();
                if (line == "quit") break;
                ip.EvalString(line);
                Console.WriteLine();
            }
        }

        public Interpreter()
        {
            globalClosure = GlobalClosure();
        }

        private Closure globalClosure;

        private enum CellType { Atom, Number, List, Primitive, Lambda, Error };
        private class Cell
        {
            public CellType Type = CellType.Atom;
            public String Val = null;
            public List<Cell> List = null;
            public Func<List<Cell>, Cell> Prim = null;
            public Closure Clo = null;
            public Cell(CellType t, string v = null)
            {
                this.Type = t;
                this.Val = v;
                if (t == CellType.List || t == CellType.Lambda)
                    this.List = new List<Cell>();
                else
                    this.List = null;
            }

            public Cell(CellType t, string v, List<Cell> l, Func<List<Cell>, Cell> f)
            {
                this.Type = t;
                this.Val = v;
                this.List = l;
                this.Prim = f;
            }

            public Cell(Func<List<Cell>, Cell> f)
            {
                this.Type = CellType.Primitive;
                this.Prim = f;
            }
        }

        private class Closure
        {
            private Dictionary<string, Cell> Env;
            public Closure Outer;
            public Closure()
            {
                this.Env = new Dictionary<string, Cell>();
                this.Outer = null;
            }

            public Cell this[string var]
            {
                get
                {
                    if (this.Env.ContainsKey(var))
                        return this.Env[var];
                    else
                        return null;
                }
                set
                {
                    if (this.Env.ContainsKey(var))
                        this.Env[var] = value;
                    else
                        this.Env.Add(var, value);
                }
            }
        }

        private List<string> Tokenize(string expr)
        {
            expr = expr.Replace("(", " ( ").Replace(")", " ) ");
            return expr.Split(' ').Where(x => !string.IsNullOrEmpty(x)).ToList();
        }

        private Cell TokensToCell(List<string> tokens)
        {
            string token = tokens[0];
            tokens.RemoveAt(0);
            if (token == "(")
            {
                Cell c = new Cell(CellType.List);
                while (tokens[0] != ")")
                    c.List.Add(TokensToCell(tokens));
                tokens.RemoveAt(0);
                return c;
            }
            else if (token[0] == '\'' || token[0] == '`')
            {
                Cell c = new Cell(CellType.List);
                c.List.Add(new Cell(CellType.Atom, "quote"));
                if (token.Length == 1)
                    c.List.Add(TokensToCell(tokens));
                else
                    c.List.Add(new Cell(CellType.Atom, token.Substring(1)));

                return c;
            }
            else
            {
                BigInteger bi;
                if (BigInteger.TryParse(token, out bi))
                    return new Cell(CellType.Number, token);
                else
                    return new Cell(CellType.Atom, token);
            }
        }

        private void ShowCell(Cell cell)
        {
            if (cell.Type == CellType.List)
            {
                Console.Write("(");
                if (cell.List.Count > 0)
                {
                    for (int i = 0; i < cell.List.Count - 1; i++)
                    {
                        ShowCell(cell.List[i]);
                        Console.Write(" ");
                    }
                    ShowCell(cell.List.Last());
                }
                Console.Write(")");
            }
            else if (cell.Type == CellType.Primitive)
            {
                Console.Write("[Primitive] ...");
            }
            else if (cell.Type == CellType.Lambda)
            {
                Console.Write("[Lambda] ");
                for (int i = 0; i < cell.List[0].List.Count; i++)
                    Console.Write(cell.List[0].List[i].Val + " ");
                Console.Write("...");
            }
            else if (cell.Type == CellType.Error)
            {
                Console.Write("[Error] " + cell.Val);
            }
            else
            {
                Console.Write(cell.Val);
            }
        }

        private Cell Eval(Cell cell, Closure closure)
        {
            if (cell.Type == CellType.Atom)
            {
                while (closure != null)
                {
                    Cell c = closure[cell.Val];
                    if (c != null) return c;
                    else closure = closure.Outer;
                }
                return new Cell(CellType.Error, "not found value of " + cell.Val);
            }
            else if (cell.Type == CellType.Number)
            {
                return new Cell(CellType.Number, cell.Val); // cell;
            }
            else if (cell.Type == CellType.Error)
            {
                return cell;
            }
            else if (cell.Type == CellType.Primitive)
            {
                return cell;
            }
            else if (cell.Type == CellType.Lambda)
            {
                return cell;
            }
            else
            {
                Cell c0 = cell.List[0];
                Cell v0 = Eval(c0, closure);
                if (v0.Type == CellType.Primitive) // apply primitive function
                {
                    List<Cell> vals = new List<Cell>();
                    for (int i = 1; i < cell.List.Count; i++)
                        vals.Add(Eval(cell.List[i], closure));
                    foreach (Cell c in vals)
                    {
                        if (c.Type == CellType.Error)
                            return c;
                    }
                    return v0.Prim(vals);
                }
                else if (v0.Type == CellType.Lambda) // apply lambda function
                {
                    Cell vars = v0.List[0];
                    Cell body = v0.List.Last();

                    Closure clo;
                    clo = new Closure();
                    clo.Outer = closure;

                    if (vars.List.Count != cell.List.Count - 1)
                        return new Cell(CellType.Error, "lambda arguments expect " + vars.List.Count);

                    for (int i = 0; i < vars.List.Count; i++)
                        clo[vars.List[i].Val] = Eval(cell.List[i + 1], closure);

                    for (int i = 1; i < v0.List.Count - 1; i++)
                        Eval(v0.List[i], clo);

                    return Eval(body, clo);
                }
                else if (v0.Type == CellType.Number) // (0 1 2)
                {
                    Cell ret = new Cell(CellType.List);
                    foreach (Cell c in cell.List)
                        ret.List.Add(Eval(c, closure));
                    return ret;
                }
                else if (c0.Type == CellType.Atom)
                {
                    if (c0.Val == "define")
                    {
                        Cell c1 = cell.List[1];
                        if (c1.Type == CellType.Atom) // define var form
                        {
                            Closure clo = new Closure();
                            clo.Outer = closure;
                            for (int i = 1; i < cell.List.Count - 1; i++)
                            {
                                Eval(cell.List[i], clo);
                            }
                            Cell ret = Eval(cell.List.Last(), clo);
                            closure[c1.Val] = ret;
                            ret.Clo = clo;
                            return ret;
                        }
                        else if (c1.Type == CellType.List)
                        {
                            Closure clo = new Closure();
                            clo.Outer = closure;
                            Cell cl = new Cell(CellType.List);
                            Cell vl = new Cell(CellType.List);
                            for (int i = 1; i < c1.List.Count; i++) vl.List.Add(c1.List[i]);
                            cl.List.Add(new Cell(CellType.Atom, "lambda"));
                            cl.List.Add(vl);
                            for (int i = 2; i < cell.List.Count; i++) cl.List.Add(cell.List[i]);
                            Cell ret = Eval(cl, closure);
                            closure[c1.List[0].Val] = ret;
                            ret.Clo = clo;
                            return ret;
                        }
                    }
                    else if (c0.Val == "if")
                    {
                        Cell test = Eval(cell.List[1], closure);
                        if (cell.List.Count > 4 || cell.List.Count < 3)
                            return new Cell(CellType.Error, "expression of if");
                        if (test.Type == CellType.Atom && test.Val == "#t")
                        {
                            return Eval(cell.List[2], closure);
                        }
                        else if (test.Type == CellType.Atom && test.Val == "#f")
                        {
                            if (cell.List.Count == 4)
                                return Eval(cell.List[3], closure);
                            else return globalClosure[""];
                        }
                    }
                    else if (c0.Val == "quote")
                    {
                        if (cell.List.Count < 2) return closure[""];
                        return cell.List[1];
                    }
                    else if (c0.Val == "case")
                    {

                    }
                    else if (c0.Val == "cond")
                    {
                        for (int i = 1; i < cell.List.Count - 1; i++)
                        {
                            Cell clause = cell.List[i];
                            if (clause.Type == CellType.List &&
                                Eval(clause.List[0], closure).Val == "#t")
                            {
                                if (clause.List.Count == 1) return globalClosure[""];
                                for (int j = 1; j < clause.List.Count - 1; j++)
                                {
                                    Eval(clause.List[j], closure);
                                }
                                return Eval(clause.List.Last(), closure);
                            }
                        }
                        if (cell.List.Last().Type == CellType.List)
                        {
                            if (cell.List.Last().List[0].Val == "else")
                            {
                                if (cell.List.Last().List.Count == 1) return globalClosure[""];
                                for (int j = 1; j < cell.List.Last().List.Count - 1; j++)
                                    Eval(cell.List.Last().List[j], closure);
                                return Eval(cell.List.Last().List.Last(), closure);
                            }
                        }
                    }
                    else if (c0.Val == "lambda")
                    {
                        Cell c1 = cell.List[1];
                        if (c1.Type == CellType.Atom)
                        {
                            Cell ret = new Cell(CellType.Lambda);
                            Cell vars = new Cell(CellType.List);
                            vars.List.Add(c1);
                            ret.List.Add(vars);
                            for (int i = 2; i < cell.List.Count; i++)
                                ret.List.Add(cell.List[i]);
                            return ret;
                        }
                        else if (c1.Type == CellType.List)
                        {
                            Cell ret = new Cell(CellType.Lambda);
                            for (int i = 1; i < cell.List.Count; i++)
                                ret.List.Add(cell.List[i]);
                            return ret;
                        }
                    }
                    else if (c0.Val == "set!")
                    {

                    }
                    else if (c0.Val == "begin")
                    {
                        for (int i = 1; i < cell.List.Count - 1; i++)
                            Eval(cell.List[i], closure);
                        return Eval(cell.List.Last(), closure);
                    }
                    else if (c0.Val == "let")
                    {

                    }
                }

                return new Cell(CellType.Error, "unknown form");
            }
        }


        public void EvalString(string expr)
        {
            Cell cell = TokensToCell(Tokenize(expr));
            Cell res = Eval(cell, globalClosure);
            ShowCell(res);
        }

        private Closure GlobalClosure()
        {
            Closure clo = new Closure();

            clo["nil"] = new Cell(CellType.Atom, "nil");
            clo["false"] = new Cell(CellType.Atom, "#f");
            clo["true"] = new Cell(CellType.Atom, "#t");
            clo[""] = new Cell(CellType.Atom, "");
            clo["+"] = new Cell(FuncAdd);
            clo["-"] = new Cell(FuncMinus);
            clo["*"] = new Cell(FuncMulti);
            clo["/"] = new Cell(FuncDivide);
            clo["<"] = new Cell(FuncLess);
            clo[">"] = new Cell(FuncGreater);
            clo["<="] = new Cell(FuncLessEqual);
            clo[">="] = new Cell(FuncGreaterEqual);
            clo["="] = new Cell(FuncEqual);
            clo["car"] = new Cell(FuncCar);
            clo["cdr"] = new Cell(FuncCdr);
            clo["length"] = new Cell(FuncLength);
            clo["list"] = new Cell(FuncList);
            clo["null?"] = new Cell(FuncIsNull);
            clo["list?"] = new Cell(FuncIsList);

            return clo;
        }

        private Cell FuncAdd(List<Cell> cl)
        {
            BigInteger bi = BigInteger.Parse(cl[0].Val);
            for (int i = 1; i < cl.Count; i++) bi += BigInteger.Parse(cl[i].Val);
            return new Cell(CellType.Number, bi.ToString());
        }

        private Cell FuncMinus(List<Cell> cl)
        {
            BigInteger bi = BigInteger.Parse(cl[0].Val);
            for (int i = 1; i < cl.Count; i++) bi -= BigInteger.Parse(cl[i].Val);
            return new Cell(CellType.Number, bi.ToString());
        }

        private Cell FuncMulti(List<Cell> cl)
        {
            BigInteger bi = BigInteger.Parse(cl[0].Val);
            for (int i = 1; i < cl.Count; i++) bi *= BigInteger.Parse(cl[i].Val);
            return new Cell(CellType.Number, bi.ToString());
        }

        private Cell FuncDivide(List<Cell> cl)
        {
            BigInteger bi = BigInteger.Parse(cl[0].Val);
            for (int i = 1; i < cl.Count; i++) bi /= BigInteger.Parse(cl[i].Val);
            return new Cell(CellType.Number, bi.ToString());
        }

        private Cell FuncLess(List<Cell> cl)
        {
            if (cl.Count != 2) return new Cell(CellType.Error, "parameters for <");
            if (BigInteger.Parse(cl[0].Val) < BigInteger.Parse(cl[1].Val))
                return globalClosure["true"];
            else return globalClosure["false"];
        }

        private Cell FuncGreater(List<Cell> cl)
        {
            if (cl.Count != 2) return new Cell(CellType.Error, "parameters for >");
            if (BigInteger.Parse(cl[0].Val) > BigInteger.Parse(cl[1].Val))
                return globalClosure["true"];
            else return globalClosure["false"];
        }

        private Cell FuncLessEqual(List<Cell> cl)
        {
            if (cl.Count != 2) return new Cell(CellType.Error, "parameters for <=");
            if (BigInteger.Parse(cl[0].Val) <= BigInteger.Parse(cl[1].Val))
                return globalClosure["true"];
            else return globalClosure["false"];
        }

        private Cell FuncGreaterEqual(List<Cell> cl)
        {
            if (cl.Count != 2) return new Cell(CellType.Error, "parameters for >=");
            if (BigInteger.Parse(cl[0].Val) >= BigInteger.Parse(cl[1].Val))
                return globalClosure["true"];
            else return globalClosure["false"];
        }

        private Cell FuncEqual(List<Cell> cl)
        {
            if (cl.Count != 2) return new Cell(CellType.Error, "parameters for =");
            if (BigInteger.Parse(cl[0].Val) == BigInteger.Parse(cl[1].Val))
                return globalClosure["true"];
            else return globalClosure["false"];
        }

        private Cell FuncCar(List<Cell> cl)
        {
            if (cl == null || cl[0].List == null || cl[0].List.Count == 0) return new Cell(CellType.Error, "null list for car");
            return Eval(cl[0].List[0], cl[0].List[0].Clo);
        }

        private Cell FuncCdr(List<Cell> cl)
        {
            if (cl == null || cl[0].List == null || cl[0].List.Count == 0) return new Cell(CellType.Error, "null list for car");
            if (cl[0].List.Count > 0)
            {
                Cell ret = new Cell(CellType.List);
                for (int i = 1; i < cl[0].List.Count; i++)
                    ret.List.Add(Eval(cl[0].List[i], cl[0].List[i].Clo));
                return ret;
            }
            return new Cell(CellType.List);
        }

        private Cell FuncLength(List<Cell> cl)
        {
            if (cl == null || cl[0].List == null) return new Cell(CellType.Error, "null list for length");
            return new Cell(CellType.Number, cl[0].List.Count.ToString());
        }

        private Cell FuncList(List<Cell> cl)
        {
            Cell ret = new Cell(CellType.List);
            foreach (Cell c in cl)
            {
                ret.List.Add(Eval(c, c.Clo));
            }
            return ret;
        }

        private Cell FuncIsNull(List<Cell> cl)
        {
            if (cl == null || cl[0].List == null) return new Cell(CellType.Error, "null list for null?");
            if (cl[0].Type != CellType.List) return new Cell(CellType.Error, "parameter for null? is not list");
            if (cl[0].List.Count == 0) return globalClosure["#f"];
            return globalClosure["#t"];
        }

        private Cell FuncIsList(List<Cell> cl)
        {
            if (cl == null || cl[0].List == null ) return new Cell(CellType.Error, "parameter for list? is wrong");
            if (cl[0].Type != CellType.List) return globalClosure["#f"];
            return globalClosure["#t"];
        }

    }
}
