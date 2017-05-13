package model

import scala.collection.mutable

/**
  * Created by sparr on 2017/5/12.
  */
abstract class NerveNet[T](top: NerveTop[T]) extends Serializable{
  /**
    * 学习速度
    */
  var a = 0.5

  /**
    * 每一层的误差
    *   输出层的误差由期望输出决定
    *   其它层的误差由其下一层的误差决定
    */
  var err : Array[Array[Double]] = _
  /**
    * 单元的实际的输出
    * realOutput(0)层是输入层,该层的值是已知
    */
  var realOutput : Array[Array[Double]] = _

  /**
    * 输出(最后)层的期望输出
    * expOutput(i)(j)表示第i个用例的第j个神经元的期望输出,一开始是已知的
    */
  var expOutput:Array[Array[Int]] = _


  /**
    * 训练用例
    */
  var trainCase : Array[Array[Double]] = _


  /**
    * 结果代表的类别
    */
  var resultClass : Map[String,String] = _

  /**
    * sigmoid 函数
    * @param x 输入的 自变量
    * @return  1/(1+exp(-x))
    */
  protected def sigmoid(x:Double):Double = 1.0/(1+Math.exp(-x))

  /**
    * 获取节点在结构中的位置
    * @param nerveNode 节点的结构
    * @param nodeName 节点的名称
    * @return (layer,index)
    */
  protected def getPosition(nerveNode:Array[Array[T]],nodeName:T):(Int,Int) = {
    for(i <- nerveNode.indices;
        j<- nerveNode(i).indices)
      if(nerveNode(i)(j)==nodeName)
        return (i,j)
    (-1,-1)
  }

  /**
    * 清空上一次的实际输出结果
    */
  protected def clearOutput():Unit = {
    for( i<- realOutput.indices;j<- realOutput(i).indices)
      realOutput(i)(j) = 0.0
  }

  /**
    * 计算实际输出
    */
  protected def calOutput():Unit = {
    for( i<- 1 until top.nerveNode.length){ //第i层神经网络
      for( j<- top.nerveNode(i).indices){   //第j个节点
        for( k<- top.nerveNode(i-1).indices){ //对上一层的每个节点k计算权值
        val v = top.weight((top.nerveNode(i-1)(k),top.nerveNode(i)(j)))*realOutput(i-1)(k)
          realOutput(i)(j) += v
          //          println(s"($i,$j) v = $v")
        }
        //        printRealOutput()
        realOutput(i)(j) -= top.xita(top.nerveNode(i)(j))
        //        println("realoutput(i)(j) = " + realOutput(i)(j))
        realOutput(i)(j) = sigmoid(realOutput(i)(j))
      }
    }

  }

  /**
    * 清空误差
    */
  protected def clearError():Unit = {
    for( i<-err.indices;j<- err(i).indices)
      err(i)(j) = 0.0
  }

  /**
    * 计算节点误差
    */
  protected def calError(currentCase:Int):Unit = {
    clearError()
    for(i <- err.indices.reverse if i>0){  // 最后一层开始往前,从输出层往前
      for(j <- err(i).indices){     // 对于每一个节点
        if(i == err.length-1) {
          //如果是输出层,期望输出已知
          val v = realOutput(i)(j) * (1 - realOutput(i)(j)) *
            (expOutput(currentCase)(j) - realOutput(i)(j))
          err(i)(j) +=v
          //          println(s"($i,$j) v = $v")
        }
        else{                     // 不是输出层,误差由下一层的误差计算而来
          for( k <- err(i+1).indices){
            val v = err(i+1)(k)*top.weight((top.nerveNode(i)(j),top.nerveNode(i+1)(k)))
            err(i)(j) += v
            //            println(s"($i,$j) v = $v")
          }
          //          printErr()
          err(i)(j) *= realOutput(i)(j)*(1-realOutput(i)(j))
          //          println("err(i)(j) = "+err(i)(j))
        }
      }
    }

  }

  /**
    * 更新权重
    */
  protected def updateWeight(): Unit ={
    for( (i,j) <- top.weight.keys){ // 对于Wij
    val (layer1,index1) = getPosition(top.nerveNode,i)//获取i节点在网络中的位置
    val (layer2,index2) = getPosition(top.nerveNode,j)//获取j节点在网络中的位置
      //      print(s"$i to $j += ")
      //      print(s"($layer1 $index1) to ")
      //      println(s"($layer2 $index2)")

      val v = a*realOutput(layer1)(index1)*err(layer2)(index2)
      top.weight((i,j)) = top.weight((i,j)) + v
      //      println(v)
    }
  }

  /**
    * 更新xita
    */
  protected def updateXita() : Unit = {
    for( i <- top.xita.keys) {// 对于Xita(i)
    val (layer,index) = getPosition(top.nerveNode,i)
      val v = -a*err(layer)(index)
      top.xita(i) += v
      //      println(s"^xita($i) = $v")
    }
  }

  /**
    * 打印每一层的实际输出
    */
  def printRealOutput():Unit = {
    println("-------------realoutput------------")
    for( i<- realOutput.indices) {
      for (j <- realOutput(i).indices) {
        printf("%.8f\t",realOutput(i)(j))
      }
      println()
    }
  }

  /**
    * 打印xita的值
    */
  def printXita() : Unit = {
    println("xita : "+top.xita)
  }

  /**
    * 打印误差
    */
  def printErr():Unit = {
    println("------------error--------------")
    for( i<- err.indices) {
      for (j <- err(i).indices) {
        printf("%.6f\t",err(i)(j))
      }
      println()
    }
  }

  /**
    * 打印权重
    */
  def printWeight():Unit = {
    println("------------weight--------------")
    for((node1,node2) <- top.weight.keys)
      println("("+node1,node2+")  "+top.weight((node1,node2)))
  }

  /**
    * 打印训练用例
    */
  def printTrainCase():Unit = {
    println("-------------traincase------------")
    for( i<- trainCase.indices) {
      for (j <- trainCase(i).indices) {
        printf("%.3f\t",trainCase(i)(j))
      }
      println()
    }
  }

  def print(): Unit ={
    printWeight()
    printXita()
  }
}

object NerveNet{

  /*未训练的实例*/
  def getInstance[A](
               top:NerveTop[A],
               trainCase:Array[Array[Double]],
               expOutput:Array[Array[Int]],
               a:Double,
               resultClass:Map[String,String]
              ): UnTrainNerveNet[A] = {
    val nerveNet = new UnTrainNerveNet[A](top)
    nerveNet.trainCase = trainCase
    nerveNet.realOutput = top.nerveNode.map( //复制nerveNode的结构
      (arr)=>{
        arr.map((_) =>{
          0.0
        })
      }
    )
    nerveNet.err = top.nerveNode.map( //复制nerveNode的结构
      (arr)=>{
        arr.map((_) =>{
          0.0
        })
      }
    )
    nerveNet.expOutput = expOutput
    nerveNet.a = a
    nerveNet.resultClass = resultClass
    nerveNet
  }
}



