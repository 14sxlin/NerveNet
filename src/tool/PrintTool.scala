package tool

import scala.collection.mutable


object PrintTool {

  def printArray[T](array: Array[Array[T]]){
    for( i<- array.indices) {
      for (j <- array(i).indices){
        print(array(i)(j)+" ")
      }
      println()
    }
  }

  def printMap[A,B](map:mutable.Map[A,B]): Unit ={
    for( key <- map.keys){
      println(key+" "+map(key))
    }
  }

}
