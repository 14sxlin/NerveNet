package model

import java.util.NoSuchElementException

/**
  * Created by sparr on 2017/5/12.
  */
@SerialVersionUID(512)
class TrainedNerveNet[T](top: NerveTop[T])
  extends NerveNet(top) with Serializable{


  /**
    * 预测输入的类别
    * @param input 输入节点的输入
    * @return 结果四舍五入,每个节点只输出0或1
    */
  def predict(input:Array[Double]): String = {
    for( i <- input.indices)
      realOutput(0)(i) = input(i)
    calOutput()
//    printRealOutput()
    realOutput(realOutput.length-1).map(Math.round).foldLeft("")(_+_)
  }

  /**
    * 根据计算结果获取分类
    * @param result 分类字符串
    * @return
    */
  def getLabelName(result:String):String = {
    try {
      resultClass(result)
    }catch {
      case _:NoSuchElementException =>
        "unknown"
    }
  }

  /**
    * 转换成未训练的神经网络
    * @param trainCase 训练用例
    * @return
    */
  def toUnTrainNerveNet(trainCase:Array[Array[Double]]): UnTrainNerveNet[T] = {
    NerveNet.getInstance(top,trainCase,expOutput,a,resultClass)
  }

  /**
    * 通过测试集计算预测的准确率
    * @param testCase 测试的数据
    * @param expClass 正确的类标号
    * @return
    */
  def test(testCase:Array[Array[Double]],expClass:Array[String]): String = {
    var right = 0
    var i = 0
    for(tcase <- testCase){
      val r = predict(tcase)
      if(expClass(i)==getLabelName(r))
        right += 1
      printf("%s : %s\n",expClass(i),getLabelName(r))
      i += 1
    }
    ""+right*1.0/testCase.length*100+"%"

  }

}
