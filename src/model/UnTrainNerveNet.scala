package model

/**
  * Created by sparr on 2017/5/12.
  */
@SerialVersionUID(512)
case class UnTrainNerveNet[T](top: NerveTop[T])
  extends  NerveNet(top) with Serializable {

  private[this] var currentCase = 0 // 指代当前训练使用的是哪个用例

  /**
    * 每轮的最多迭代次数
    */
  var totalRound = 10000

  /**
    * 当前的迭代次数
    */
  var round = 0


  /**
    * 误差精确度
    */
  var accuracy = 0.001


  /**
    * 计算误差和
    */
  def sumError():Double = {
    var sum = 0.0
    val last = realOutput.length-1
    for(j <- realOutput(last).indices)
    {
      sum += Math.pow(expOutput(currentCase)(j) - realOutput(last)(j),2)
    }
    println(s"round = $round  error = $sum")
    sum
  }

  /**
    * 判断训练是否应该结束
    * @return
    */
  private def hasFinish : Boolean = {
    if(round>0 && (round>=totalRound || sumError()<=accuracy)) {//
      println("**************** round = " + round)
      return true
    }
    false
  }

  /**
    * 训练模型
    */
  def train():Unit = {
    while(!hasFinish){
      clearOutput()
      for( i <- trainCase(currentCase).indices) //输入训练用例
        realOutput(0)(i) = trainCase(currentCase)(i)
      calOutput()
      calError(currentCase)
      updateWeight()
      updateXita()
      round += 1
      currentCase += 1
      currentCase %= trainCase.length
      //      a = 1.0/(round+1)
      //      printRealOutput()
      //      printErr()
      //      printWeight()
      //      printXita()
    }
  }

  def toTrainedNerveNet:TrainedNerveNet[T] = {
    val trainedNerveNet = new TrainedNerveNet[T](top)
    trainedNerveNet.expOutput = expOutput
    trainedNerveNet.resultClass = resultClass
    trainedNerveNet.err = err
    trainedNerveNet.realOutput = realOutput
    trainedNerveNet
  }

}
