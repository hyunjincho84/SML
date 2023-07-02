#include "List.h"
#include <exception> 
#include <functional> 
#include <utility> 
#include <iostream> 

template<class T, class U>
std::function<std::function<U(List<T>)>(U)>
cfoldl(std::function<U(U, T)> f) 
{
    std::function<std::function<U(List<T>)>(U)> captured1;
    captured1 = [=](U acc) {
      std::function<U(List<T>)> captured2;
      captured2 = [=](List<T> lst) {
        if (lst.isEmpty()) return acc;
        else return cfoldl(f) (f(acc, lst.head())) (lst.tail());
      };
      return captured2;
    };
    return captured1;
}

using std::pair;

auto compose = [](auto f, auto g) { 
    return [=](auto&& ... x) { 
        return f(g(x...)); 
    }; 
};


int main() {
  int x = 42;
  auto addX = [x](int y) { return x+y;};
  std::cout << "addX(10):" << addX(10) << std::endl;
  x = 1;
  std::cout << "addX(10):" << addX(10) << std::endl;

  int y = 42;
  auto addY = [&](int x) { return x+y;};
  std::cout << "addY(10):" << addY(10) << std::endl;
  y = 1;
  std::cout << "addY(10):" << addY(10) << std::endl;


  List<int> l;
  l = l.cons(0).cons(1).cons(2).cons(3).cons(4);
  int sum = foldl([](int a, int b) { return a+b;}, 0, l);

  int max = foldl([](int a, int b) { return a>b?a:b;}, 0, l);

  int val = 3;
  bool anyLargerThanVal = foldl([=](bool acc, int x) { return x>val? true:acc;}, false, l);

  
  std::cout << "sum:" << sum << ", max:" << max << std::endl;
  std::cout << "anyLargerThanVal:" << anyLargerThanVal << std::endl;

  auto h1 = compose([](int x){return x+1;}, [](){ return 42;});
  std::cout << "h1()=" << h1() << std::endl;

  auto h2 = compose([](pair<int,int> p){return p.first+p.second;}, [](int x, int y){ return pair(x, y);});
  std::cout << "h2(1,2)=" << h2(1, 2) << std::endl;


  std::function<int(int,int)> adder = [](int a, int b)->int { return a+b;};
  auto sumFold = cfoldl(adder);
  std::cout << "after sumFold \n";
  int sum2 = sumFold(100)(l);
  std::cout << "sum2:" << sum2 << std::endl;

  return 0;
}

