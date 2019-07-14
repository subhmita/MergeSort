package com.subhmita

object MergeSort {  
  
def main(args : Array[String]):Unit = {
   val num = List(4,6,2,51,22)
  def msort[T](xs :List[T])(implicit ord :Ordering[T]): List[T] =  {
  val n =xs.length/2
  if (n== 0) xs
 else {
 def merge(xs :List[T],ys :List[T]) :List[T] = (xs,ys) match {
 case  (Nil,ys)  => ys
  case  (xs,Nil)  => xs
  case (x :: xs1,y :: ys1) =>
  if(ord.lt(x,y)) x:: merge(xs1,ys)
  else y::merge(xs,ys1)
 }
 val(fst,scnd) = xs splitAt(n)
 merge(msort(fst),msort(scnd))
  }
 }
   println(msort(num))
  
  }
}