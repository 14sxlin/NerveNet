package io

import java.io._

import model.{NerveNet, NerveTop, TrainedNerveNet, UnTrainNerveNet}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by sparr on 2017/5/1.
  */
class NerveNetIO {

  val NERVENODE = "nerveNode"
  val WEIGHT = "weight"
  val XITA = "xita"
  val A = "studyRate"
  val RESULTCLASS = "resultClass"


  var currentNerveNet:NerveNet[String]= _

  private var class2Code : mutable.Map[String,Array[Int]] = _
  private var nerveNode : ArrayBuffer[Array[String]] = _
  private var a = 0.0
  private var xita : mutable.Map[String,Double] = _
  private var weight : mutable.Map[(String,String),Double] = _
  private var resultClass : mutable.Map[String,String] = _
  private var nerveTop:NerveTop[String] = _
  private var trainCases : ArrayBuffer[Array[Double]] = _
  private var expOutput : ArrayBuffer[Array[Int]] = _

  var testInput : ArrayBuffer[Array[Double]] = _
  var testExpClass : ArrayBuffer[String] = _

  var predictCase : ArrayBuffer[Array[Double]] = _

  private def loadData(nerveFile:File): Unit = {
    nerveNode = new ArrayBuffer[Array[String]]()
    xita = mutable.Map[String,Double]()
    weight = mutable.Map[(String,String),Double]()
    resultClass = mutable.Map[String,String]()
    class2Code = mutable.Map[String,Array[Int]]()

    val source = Source.fromFile(nerveFile,"utf-8")
    val content = source.mkString
    val sep =
      if(System.getProperty("os.name").toLowerCase.startsWith("win"))
        "\r\n"
      else "\n"   // 换行符号  windows 和 linux 不一样

    for( part <- content.split(";")){
      part match {
        case _ if part.contains(NERVENODE) =>
          for( line <- part.split(sep)
               if !line.contains("#") && line!=""){
            //            println("line = " +line)
            nerveNode += line.split(" ")
          }
        case _ if part.contains(A) =>
          for( line <- part.split(sep)
               if !line.contains("#") && line!=""){
            //            println("line = '" +line+"'")
            a = line.toDouble
          }
        case _ if part.contains(WEIGHT) =>
          for( line <- part.split(sep)
               if !line.contains("#") && line!=""){
            //            println("line = '" +line+"'")
            val temp = line.split(" ")
            weight += (temp(0),temp(1)) -> temp(2).toDouble
          }
        case _ if part.contains(XITA) =>
          for( line <- part.split(sep)
               if !line.contains("#") && line!=""){
            //            println("line = '" +line+"'")
            val temp = line.split(" ")
            xita += temp(0) -> temp(1).toDouble
          }
        case _ if part.contains(RESULTCLASS) =>
          for( line <- part.split(sep)
               if !line.contains("#") && line!=""){
            //            println("line = '" +line+"'")
            val temp = line.split(" ")
            resultClass += temp(0) -> temp(1)
            class2Code += temp(1) -> temp(0).toCharArray.map(c => {c-'0'})
          }

        case _ => throw new Exception("unknown input")
      }
    }
  }


  /**
    * 读取文件中的拓扑结构
    * @param nerveFile 拓扑结构的文件
    */
  def loadTop(nerveFile: File):Unit = {
    loadData(nerveFile)
    nerveTop = new NerveTop[String]()
    nerveTop.nerveNode = nerveNode.toArray
    nerveTop.weight = weight
    nerveTop.xita = xita
  }

  private def readTrainCase(file: File, sep:String=",") = {
    trainCases = new ArrayBuffer[Array[Double]]()
    expOutput = new ArrayBuffer[Array[Int]]()
    for(line <- Source.fromFile(file,"utf-8").getLines){
      val datas = line.split(sep)
      trainCases += datas.slice(0,datas.length-1).map(_.toDouble)
      expOutput += class2Code(datas.last)
    }
  }

  /**
    * 加载数据 , currentNerve 变成 untrainedNerve的实例
    */
  def loadTrainCase(file: File, sep:String=","):Unit = {
    readTrainCase(file,sep)
    currentNerveNet = NerveNet.getInstance(
      nerveTop,
      trainCases.toArray,
      expOutput.toArray,
      a,
      resultClass.toMap
    )
  }

  /**
    * 读取测试用例
    * @param testFile 测试用例的文件
    * @param sep 分隔符,默认是 ,
    */
  def loadTestCase(testFile:File,sep:String=","): Unit = {
    testInput = new ArrayBuffer[Array[Double]]()
    testExpClass = new ArrayBuffer[String]()
    for(line <- Source.fromFile(testFile,"utf-8").getLines){
      val datas = line.split(sep)
      testInput += datas.slice(0,datas.length-1).map(_.toDouble)
      testExpClass += datas(datas.length-1)
    }
  }

  /**
    * 从文件中读取预测集
    * @param preFile 包含要预测的数据
    * @param sep 分隔符
    */
  def loadPredictCase(preFile:File,sep:String=","){
    predictCase = new ArrayBuffer[Array[Double]]()
    for(line <- Source.fromFile(preFile,"utf-8").getLines){
      predictCase += line.split(sep).map(_.toDouble)
    }
  }
  /**
    * 将已经训练好的神经网络保存到文件中
    */
  def save(file:File): Unit ={
    val objOut = new ObjectOutputStream(new FileOutputStream(file))
    objOut.writeObject(
      currentNerveNet.asInstanceOf[UnTrainNerveNet[String]]
        .toTrainedNerveNet
    )
    objOut.flush()
    objOut.close()
  }

  /**
    * 从文件中读取训练好的模型
    * currentNerve 变成 TrainedNerveNet的实例
    * @param file 模型保存的文件
    * @return
    */
  def load(file:File):Unit = {
    val objIn = new ObjectInputStream(new FileInputStream(file))
    currentNerveNet =  objIn.readObject().asInstanceOf[TrainedNerveNet[String]]
    objIn.close()
  }


  /**
    * 对已经训练好的模型再训练
    * @param savedFile 保存了模型的文件
    * @param accuracy 精度
    */
  def retrain(topFile:File,savedFile:File,trainFile:File,accuracy:Double): Unit = {
    loadTop(topFile)
    readTrainCase(trainFile)
    load(savedFile)

    currentNerveNet match {
      case trained:TrainedNerveNet[String] =>
        val t = trained.toUnTrainNerveNet(trainCases.toArray)
        t.accuracy = accuracy
        t.train()
        currentNerveNet = t
      case unTrainNerveNet:UnTrainNerveNet[String] =>
        unTrainNerveNet.accuracy = accuracy
        unTrainNerveNet.train()
        currentNerveNet = unTrainNerveNet
    }

  }
}
