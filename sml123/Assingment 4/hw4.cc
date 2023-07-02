#include <string>
#include <iostream>
#include <variant>
#include <map>
#include <exception>
#include <stdexcept>
#include "box.h"
#include "List.h"

using std::variant;
using std::string;

// Definition of Expr variants
struct Var {
    string name;
    Var(string _name): name(_name) {};
    operator std::string()
    const {
        return "Var(" + name + ")";
    }
};
struct Int {
    int val;
    Int(int _val): val(_val) {};
    operator std::string()
    const {
        return "Int(" + std::to_string(val) + ")";
    }
};
struct AUnit {
    AUnit() {};
    operator std::string()
    const {
        return "AUnit()";
    }
};
using Expr = variant<Var, Int, AUnit, box<struct IsAUnit>, box<struct Add>, box<struct IfGreater>, box<struct MLet>, box<struct Fun>, box<struct Closure>, box<struct APair>, box<struct Fst>, box<struct Snd>, box<struct Call>>;

template < typename T > bool is(Expr e);

std::string toString(Expr e);

struct Add {
    Expr e1,
    e2;
    Add(Expr _e1, Expr _e2): e1(_e1),
    e2(_e2) {};
    operator std::string()
    const {
        return "Add(" + toString(e1) + ", " + toString(e2) + ")";
    }
};
struct IfGreater {
    Expr e1,
    e2,
    e3,
    e4;
    IfGreater(Expr _e1, Expr _e2, Expr _e3, Expr _e4): e1(_e1),
    e2(_e2),
    e3(_e3),
    e4(_e4) {};
    operator std::string()
    const {
        return "IfGreater(" + toString(e1) + ", " + toString(e2) + ", " +
                toString(e3) + ", " + toString(e4) + ")";
    }
};
struct MLet {
    string varName;
    Expr e1, e2;
    MLet(string _varName, Expr _e1, Expr _e2): varName(_varName),
    e1(_e1),
    e2(_e2) {};
    operator std::string()
    const {
        return "MLet(" + varName + ", " + toString(e1) + ", " + toString(e2) + ")";
    }
};
struct APair {
    Expr e1,
    e2;
    APair(Expr _e1, Expr _e2): e1(_e1),
    e2(_e2) {};
    operator std::string()
    const {
        return "APair(" + toString(e1) + ", " + toString(e2) + ")";
    }
};
struct Fst {
    Expr e;
    Fst(Expr _e): e(_e) {};
    operator std::string()
    const {
        return "Fst(" + toString(e) + ")";
    }
};
struct Snd {
    Expr e;
    Snd(Expr _e): e(_e) {};
    operator std::string()
    const {
        return "Snd(" + toString(e) + ")";
    }
};
struct IsAUnit {
    Expr e;
    IsAUnit(Expr _e): e(_e) {};
    operator std::string()
    const {
        return "IsAUnit(" + toString(e) + ")";
    }
};

struct Fun {
    string funName;
    string argName;
    Expr body;
    Fun(string _f, string _a, Expr _b): funName(_f),
    argName(_a),
    body(_b) {};
    operator std::string()
    const {
        return "Fun(" + funName + ", " + argName + ", " + toString(body) + ")";
    }
};
struct Closure {
    std::map<string, Expr> env;
    Fun f;
    Closure(std::map<string, Expr> _env, Fun _f): env(_env),
    f(_f) {};
    operator std::string()
    const {
        return "Closure(env, " + std::string(f) + ")";
    }
};
struct Call {
    Expr funExpr,
    actual;
    Call(Expr _fe, Expr _a): funExpr(_fe),
    actual(_a) {};
    operator std::string()
    const {
        return "Call(" + toString(funExpr) + ", " + toString(actual) + ")";
    }
};
// End of Definition of Expr variants Functions for check variants. e.g.
// is<APair>(e) or is<Int>(Expr(Int(42)))
template < typename T > bool is(Expr e) {
    return std::holds_alternative<T>(e);
}
template<>
bool is<Closure>(Expr e) {
    return std::holds_alternative < box < struct Closure >> (e);
}
template<>
bool is<IsAUnit>(Expr e) {
    return std::holds_alternative < box < struct IsAUnit >> (e);
}
template<>
bool is<Add>(Expr e) {
    return std::holds_alternative < box < struct Add >> (e);
}
template<>
bool is<IfGreater>(Expr e) {
    return std::holds_alternative < box < struct IfGreater >> (e);
}
template<>
bool is<MLet>(Expr e) {
    return std::holds_alternative < box < struct MLet >> (e);
}
template<>
bool is<Fun>(Expr e) {
    return std::holds_alternative < box < struct Fun >> (e);
}
template<>
bool is<APair>(Expr e) {
    return std::holds_alternative < box < struct APair >> (e);
}
template<>
bool is<Fst>(Expr e) {
    return std::holds_alternative < box < struct Fst >> (e);
}
template<>
bool is<Snd>(Expr e) {
    return std::holds_alternative < box < struct Snd >> (e);
}
template<>
bool is<Call>(Expr e) {
    return std::holds_alternative < box < struct Call >> (e);
}

// Converting Expr to std::string representation.
std::string toString(Expr e) {
    if (is<Int>(e)) {
        return std::get<Int>(e);
    } else if (is<Var>(e)) {
        return std::get<Var>(e);
    } else if (is<AUnit>(e)) {
        return std::get<AUnit>(e);
    } else if (is<IsAUnit>(e)) {
        return * std::get < box < struct IsAUnit >> (e);
    } else if (is < box < struct Add >> (e)) {
        Add add = * std::get < box < struct Add >> (e);
        return add;
    } else if (is < box < struct IfGreater >> (e)) {
        IfGreater ifgt = * std::get < box < struct IfGreater >> (e);
        return ifgt;
    } else if (is < box < struct MLet >> (e)) {
        MLet mlet = * std::get < box < struct MLet >> (e);
        return mlet;
    } else if (is < box < struct Fun >> (e)) {
        Fun fun = * std::get < box < struct Fun >> (e);
        return fun;
    } else if (is < box < struct Closure >> (e)) {
        Closure closure = * std::get < box < struct Closure >> (e);
        return closure;
    } else if (is < box < struct APair >> (e)) {
        return * std::get < box < struct APair >> (e);
    } else if (is < box < struct Fst >> (e)) {
        return * std::get < box < struct Fst >> (e);
    } else if (is < box < struct Snd >> (e)) {
        return * std::get < box < struct Snd >> (e);
    } else if (is < box < struct Call >> (e)) {
        Call call = * std::get < box < struct Call >> (e);
        return call;
    } else {
        throw std::runtime_error("toString(Expr): Unexpected Expr is given!");
    }
}

// Asserts that given Expr is a value in MUPL.
void assertValue(Expr e) {
    if (is<APair>(e)) {
        APair ap = * std::get < box < struct APair >> (e);
        assertValue(ap.e1);
        assertValue(ap.e2);
    } else if (!(is<Int>(e) || is<Closure>(e) || is<AUnit>(e))) {
        throw std::runtime_error(toString(e) + " is not a value!");
    }
}

// Make a new environment by copying from the passed environment.
std::map<string, Expr> makeNewEnvFrom(std::map<string, Expr> fromEnv) {
    std::map<string, Expr> newEnv(fromEnv);
    return newEnv;
}

Expr envlookup(std::map<string, Expr> env, Var v) {
    if (env.count(v.name) == 0) {
        throw std::runtime_error(
            toString(v) + " is not in the environment"
        );
    } else {
        Expr val = env.at(v.name);
        assertValue(val);
        return val;
    }
}

Expr eval_under_env(Expr e, std::map<string, Expr> env) {
    return std::visit(overload {
        [&](Int & i) {
            return e;
        },
        [&](Var & v) {
            return envlookup(env, v);
        },
        [&](box < struct Add >& a) {
            Expr e1 = eval_under_env(a -> e1, env);
            Expr e2 = eval_under_env(a -> e2, env);
            if (is<Int>(e1) && is<Int>(e2)) {
                Int i1 = std::get<Int>(e1);
                Int i2 = std::get<Int>(e2);
                Expr res(Int(i1.val + i2.val));
                return res;
            } else {
                throw std::runtime_error("Unexpected types for sub-expressions of Add");
            }
        },

        // TODO: Students need to implement following functions.
        [&](AUnit & au) {
            // std::cout<<"aunit"<<std::endl;
            Expr res = au;
            return res;
        },
        [&](box < struct IsAUnit >& isa) {
            Expr e = eval_under_env(isa -> e, env);
            if(is<AUnit>(e))
            {
                Expr res = Int(1);
                return res;
            }
            else
            {
                Expr res = Int(0);
                return res;
            }
        },
        [&](box < struct IfGreater >& ifgt) {
            Expr e1 = eval_under_env(ifgt -> e1, env);
            Expr e2 = eval_under_env(ifgt -> e2, env);
            
            if(is<Int>(e1) && is<Int>(e2))
            {
                Int i1 = std::get<Int>(e1);
                Int i2 = std::get<Int>(e2);
                if (i1.val > i2.val) 
                    return eval_under_env(ifgt -> e3, env);
                else 
                    return eval_under_env(ifgt -> e4, env);
            }
            else {
                throw std::runtime_error("Unexpected types for sub-expressions of IfGreater");
            }
        }
        ,
        [&](box < struct MLet >& l) {
            if(is<MLet>(l))
            {
                Expr e1 = eval_under_env(l -> e1, env);
                string varname = l->varName;
                env.insert_or_assign(l->varName, e1);
                Expr res = eval_under_env(l -> e2, env);
                return res;
            }
            else {
                throw std::runtime_error("Unexpected types for sub-expressions of MLet");
            }
        },
        [&](box < struct Fun >& f) {
            Expr res = Closure(env, *f);
            return res;
        },
        [&](box < struct Closure >& c) {
            return e;
        },
        [&](box < struct APair >& ap) {
            Expr e1 = eval_under_env(ap -> e1,env);
            Expr e2 = eval_under_env(ap -> e2,env);
            if(is<APair>(ap))
            {
                Expr res = APair(e1,e2);
                return res;
            }
            else
            {
                throw std::runtime_error("Unexpected types for sub-expressions of APair");
            }
        },
        [&](box < struct Fst >& fst) {
            Expr e = eval_under_env(fst -> e, env);
            if(is<APair>(e))
            {
                APair pair = * std::get < box < struct APair >> (e);
                Expr res = pair.e1;
                return res;
            }
            else 
            {
                throw std::runtime_error("Unexpected types for sub-expressions of Fst");
            }
        },
        [&](box < struct Snd >& snd) {
            Expr e = eval_under_env(snd -> e, env);
            if(is<APair>(e))
            {
                APair pair = * std::get < box < struct APair >> (e);
                Expr res = pair.e2;
                return res;
            }
            else 
            {
                throw std::runtime_error("Unexpected types for sub-expressions of Snd");
            }
        },
        [&](box < struct Call >& call) {
            Expr e1 = eval_under_env(call->funExpr, env);
            Expr e2 = eval_under_env(call->actual, env);
            if(is<Closure>(e1))
            {
                Closure c = * std::get < box < struct Closure >> (e1);
                c.env.insert_or_assign(c.f.argName, eval_under_env(call->actual,env));
                c.env.insert_or_assign(c.f.funName, eval_under_env(call->funExpr,env));
                Expr res = eval_under_env(c.f.body, c.env);
                return res;
            }
            else 
            {
                throw std::runtime_error("Unexpected types for sub-expressions of Call");
            }
        }
    }, e);
}

Expr eval(Expr e) {
    std::map<string, Expr> env;
    return eval_under_env(e, env);
}

Expr makeIntList(int from, int to) {
    Expr next = AUnit();
    Expr res = AUnit();
    for (int i = to - 1; i >= from; i--) {
        Expr tmp = APair(Int(i), next);
        res = tmp;
        next = tmp;
    }
    return res;
}

Expr ToMuplList(List<Expr> lst) //APair형태의 리스트를 진짜 리스트로 만드는거
{
    std::map<string, Expr> env;
    Expr res = eval_under_env(AUnit(),env);
    if(lst.isEmpty())
        return AUnit();
    else
    {
        res = APair(lst.head(),ToMuplList(lst.tail()));
        return res;
    }
}

List<Expr> FromMuplList(Expr e)// 진짜 리스트를 APair형태로 만드는것
{
    std::map<string, Expr> env;
    List<Expr> lst;
    Expr e1 = eval_under_env(e,env);
    Int i3 = std::get<Int>(eval_under_env(IsAUnit(e1),env));
    if(i3.val == 1)
    {
        return lst;
    }
    else
    {
        lst = FromMuplList(Snd(e1));
        lst = cons(eval_under_env(Fst(e1),env),lst);
        return lst;
    }
}

Expr IfAUnit(Expr e1, Expr e2, Expr e3) {
    return IfGreater(IsAUnit(e1), Int(0), e2, e3);
}

Expr MuplMap() {
    // TODO pseudo code in ML: 
    // pseudo code in ML:
    // fn fun_arg =>
    //    let fun muplrec(lst) =
    //           if IsAUnit(lst)
    //           then AUnit()
    //           else APair(fun_arg(Fst(lst)),
    //                      muplrec(Snd(lst)))             
    //    in
    //      muplrec /* UPDATED */
    //    end
    return Fun("f", "func", Fun("op", "list" ,IfAUnit(Var("list"),
                            AUnit(), 
                            APair(Call(Var("func"), Fst(Var("list"))), Call(Var("op"), Snd(Var("list")))))));
}

Expr MuplMapAddN() {
     // TODO
    // pseudo code in ML:
    // let val map = MuplMap()
    //    fn I => map(fn x => x+I)
    // end
    return MLet("map", MuplMap(), Fun("f", "n", Call(Var("map"), Fun("f", "x" , Add(Var("x"), Var("n"))))));
}

int main() {
    // Test code for eval()
//    std::map<string, Expr> env;
//    env.insert_or_assign("a", Expr(Int(40)));
//    Expr e = Add(Var("a"), Int(2));
//    Expr res = eval_under_env(e, env);
//    std::cout << toString(e) << " = " << toString(res) << std::endl; 
//    Int i = std::get<Int>(res);
//    std::cout << toString(e) << " = " << i.val << std::endl;
///*-------------------------------------------------------------------------------*/
//
//    Expr e2 = MLet("a", Int(5), MLet("b", Int(10), Add(Var("a"), Var("b"))));
//    res = eval_under_env(e2, env);
//    i = std::get<Int>(res);
//    std::cout << toString(e2) << " = " << i.val << std::endl;
///*-------------------------------------------------------------------------------*/
//
//    res = eval(e2);
//    i = std::get<Int>(res);
//    std::cout << toString(e2) << " = " << i.val << std::endl;
//
//    Expr e3 = Call(Fun("addi", "x", Add(Var("x"), Int(1))), Int(41));
//    res = eval_under_env(e3, env);
//    std::cout << toString(e3) << " = " << toString(res) << std::endl;
///*-------------------------------------------------------------------------------*/
//
//    Expr e4 = IfGreater(Int(0), Int(1), Int(42), Int(-42));
//    res = eval_under_env(e4, env);
//    std::cout << toString(e4) << " = " << toString(res) << std::endl;
///*-------------------------------------------------------------------------------*/
//
//    Expr e5 = MLet(
//        "b",
//        Int(5),
//        Add(Var("b"), MLet("b", Int(10), Add(Var("b"), Int(1))))
//    );
//    res = eval_under_env(e5, env);
//    std::cout << toString(e5) << " = " << toString(res) << std::endl;
///*-------------------------------------------------------------------------------*/
//    // Expr e6 = APair(Add(Int(0), Int(10)), APair(Int(1), Int(2)));
//    Expr e6 = APair(Add(Int(0), Int(10)), APair(Int(1), AUnit()));
//    res = eval_under_env(e6, env);
//    APair e9 = APair(Add(Int(0), Int(10)), APair(Int(1), AUnit()));
//    std::cout << toString(e6) << " = " << toString(res) << std::endl;
//    Expr e7 = Fst(e9);
//    res = eval_under_env(e7, env);
//    std::cout << toString(e7) << " = " << toString(res) << std::endl;
//    Expr e8 = Snd(e9);
//    res = eval_under_env(e8, env);
//    std::cout << toString(e8) << " = " << toString(res) << std::endl;
///*-------------------------------------------------------------------------------*/
//
   Expr e10 = makeIntList(0, 5);
   std::cout << toString(e10) << " = " << toString(e10) << std::endl;
//
   List<Expr> lst = FromMuplList(e10);
   int i = 0;
   while(!lst.isEmpty())
   {
        auto a = lst.head();
        std::cout << toString(a)<< std::endl;
        lst = lst.tail();
   }
   e10 = ToMuplList(lst);
   std::cout << toString(e10) << " = " << toString(e10) << std::endl;
//
//
//    e = AUnit();
//    Expr e11 = eval_under_env(IsAUnit(e),env);
//    std::cout << toString(e11) << " = " << toString(e11) << std::endl;
//    Int i3 = std::get<Int>(eval_under_env(IsAUnit(e),env));
//    if(i3.val == 1)
//        std::cout<<"tre";
//    std::cout << toString(Expr(Int(1))) << " = " << toString(e11) << std::endl;
//
//
//
//    // Expr e7 = ToMuplList(5);
//    // std::cout << toString(e7) << " = " << toString(e7) << std::endl;
//
//    // Expr e7 = FromMuplList(5);
//    // std::cout << toString(e7) << " = " << toString(e7) << std::endl;
//
//    Expr e12 = eval(Call(Call(MuplMapAddN(), Int(10)), makeIntList(0, 5)));
//    std::cout << toString(e12) << " = " << toString(e12) << std::endl;
/*------------------------------------------------------------------------------*/
	// List<int> l;
	// l = l.cons(0).cons(1).cons(2).cons(3).cons(4);
//	int sum = foldr([](int a, int b) { return a+b;},
//					0, l);
//
//	std::cout<<sum<<::std::endl;
//
	// int val = 3;
	// int anyLargerThanVal = foldl(
	// 		[=](int x,int y) { std::cout<<x<<std::endl;return x+y;},
	// 		0, l);
	// std::cout<<anyLargerThanVal<<::std::endl;
	// std::cout<<"--------------------"<<std::endl;
	// l = l.cons(0).cons(1).cons(2).cons(3).cons(4);
	// val = 3;

	// int anyLargerThanVal1 = foldr(
	// 		[=](int x,int y) { std::cout<<x<<std::endl;return x+y;},
	// 		0, l);
	// std::cout<<anyLargerThanVal1<<::std::endl;



//	auto f=[](int a){return a+1;};
//	auto g=[](int a,int b){return a+b;};
//	auto compose = [](auto f, auto g){
//		return [=](auto&& ... x) {
//			return f(g(x...));
//		};
//	};
//	auto res = compose(f,g);
//	std::cout<<res(3,3)<<std::endl;
//
//List<int> l;
//l = l.cons(0).cons(1).cons(2).cons(3).cons(4);
//std::function<int(int,int)> adder = [](int a, int b)->int{std::cout<<b<<std::endl;return a+ b;};
//auto sumFold = cfoldl(adder);
//int sum2 = sumFold(100)(l);
//
//std::cout<<sum2<<std::endl;


    return 0;
}
