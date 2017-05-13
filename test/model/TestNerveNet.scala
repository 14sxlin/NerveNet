package model
import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by sparr on 2017/4/27.
  */
class TestNerveNet extends FunSuite{

  val input = Array(
    Array(1.0,0.0,1.0), //输入层的输入,其他赋值为0
    Array(0.0,0.0),
    Array(0.0)
  )
  val nerveNode = Array(  //
    Array(1,2,3),
    Array(4,5),
    Array(6)
  )

  val expOutput = Array(
    Array(1)
  )

  val mistake = Array(
    Array(0.0,0.0,0.0),
    Array(0.0,0.0),
    Array(0.0)
  )
  val weight = mutable.Map(
    (1,4) -> 0.2,
    (1,5) -> -0.3,
    (2,4) -> 0.4,
    (2,5) -> 0.1,
    (3,4) -> -0.5,
    (3,5) -> 0.2,
    (4,6) -> -0.3,
    (5,6) -> -0.2
  )

  val xita = mutable.Map(4 -> -0.4,5 -> 0.2,6 -> 0.1)

  val a = 0.9

  val resultClass = Map(
    "0" -> "类1",
    "1" -> "类2"
  )

  def sigmoid(x:Double):Double = 1/(1+Math.exp(-x))

  def getPosition[T](nerveNode:Array[Array[T]],nodeName:T):(Int,Int) = {
    for(i <- nerveNode.indices;
        j<- nerveNode(i).indices)
      if(nerveNode(i)(j)==nodeName)
        return (i,j)
    (-1,-1)
  }

  test("nerve class"){
    val nerveTop = new NerveTop[Int]()
    nerveTop.xita = xita
    nerveTop.weight = weight
    nerveTop.nerveNode = nerveNode
    val nerveNet = NerveNet.getInstance(
      nerveTop,
      Array(Array(1,0,1)),
      expOutput,
      a,
      resultClass
    )
//    nerveNet.trainCase = Array(Array(1.0,0.0,1.0))
//    nerveNet.nerveNode = nerveNode
//    nerveNet.realOutput = input
//    nerveNet.xita = xita
//    nerveNet.weight = weight
//    nerveNet.err = mistake
//    nerveNet.expOutput = expOutput
//    nerveNet.a = a
//    nerveNet.resultClass = resultClass

    nerveNet.train()
//    val result = nerveNet.predict(Array(1.0,1.0,0.0))
//    println(nerveNet.getLabelName(result))
//
    nerveNet.printRealOutput()
    nerveNet.printErr()
    nerveNet.printWeight()
    nerveNet.printXita()
  }

  test("test "){
    for( i<- 1 until nerveNode.length){ //第i层神经网络
      for( j<- nerveNode(i).indices){   //第j个节点
        for( k<- nerveNode(i-1).indices){ //对上一层的每个节点k计算权值
          input(i)(j) += weight((nerveNode(i-1)(k),nerveNode(i)(j)))*input(i-1)(k)
        }
        input(i)(j) += xita(nerveNode(i)(j))
        input(i)(j) = sigmoid(input(i)(j))
      }
    }

    for(i<- input.indices) {
      for (j <- input(i).indices)
        printf("%.3f \t", input(i)(j))
      println()
    }
    println("xita : "+xita)


    //更新节点误差
    for(i <- mistake.indices.reverse){  // 最后一层开始往前,从输出层往前
      for(j <- mistake(i).indices){     // 对于每一个节点
        if(i == mistake.length-1) {
          //如果是输出层,期望输出已知
          mistake(i)(j) +=
            input(i)(j) * (1 - input(i)(j)) * (expOutput(0)(j) - input(i)(j))
//          println("mistake("+i+")("+j+") = "+mistake(i)(j))
        }
        else{                     // 不是输出层,误差由下一层的误差计算而来
          for( k <- mistake(i+1).indices){
            mistake(i)(j) +=
              mistake(i+1)(k)*weight((nerveNode(i)(j),nerveNode(i+1)(k)))
          }
//          println("mistake("+i+")("+j+") = "+mistake(i)(j))
          mistake(i)(j) *= input(i)(j)*(1-input(i)(j))
//          println("mistake("+i+")("+j+") = "+mistake(i)(j))
        }
      }
    }

    for( i<- mistake.indices) {
      for (j <- mistake(i).indices) {
        printf("%.6f\t",mistake(i)(j))
      }
      println()
    }

    //更新权重
    for( (i,j) <- weight.keys){ // 对于Wij
      val (layer1,index1) = getPosition(nerveNode,i)//获取i节点在网络中的位置
      val (layer2,index2) = getPosition(nerveNode,j)//获取j节点在网络中的位置
      println(s"w$i$j $i ($layer1,$index1)")
      println(s"w$i$j $j ($layer2,$index2)")
      weight((i,j)) += a*input(layer1)(index1)*mistake(layer2)(index2)
      println("delta = "+a*input(layer1)(index1)*mistake(layer2)(index2))
      println("weight = "+weight((i,j)))
    }
    for((node1,node2) <- weight.keys)
      println("("+node1,node2+")  "+weight((node1,node2)))

    //更新西塔
    for( i <- xita.keys) {// 对于Xita(i)
      val (layer,index) = getPosition(nerveNode,i)
      xita(i) += a*mistake(layer)(index)
    }
    println(xita)
  }

}
