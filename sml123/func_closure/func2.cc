#include "List.h"
#include <exception> 
#include <functional> 
#include <utility> 
#include <iostream> 


template<class T, class F>
bool ifall(F f, List<T> lst) {
  static_assert(std::is_convertible<F, std::function<bool(T)>>::value, 
                 "Requires a function type bool(T)");
  if (lst.isEmpty()) {
    return true;
  } 
  if (f(lst.head()))
    return ifall(f, lst.tail());
  else return false;
}

int main() {
  List<int> l;
  l = l.cons(3);
  l = l.cons(2);
  l = l.cons(1);
  l = l.cons(0);

  List<int> l3 = filter([](int x){ return x%2==0;}, l);
  std::cout << "l3:";
  print(l3);
 
  bool allOdd = ifall([](int x){ return x%2==1; }, l3);
 
  List<int> l2 = fmap<int>([](int x){ return x+1;}, l);
  std::cout << "l2:";
  print(l2);

  return 0;
}

