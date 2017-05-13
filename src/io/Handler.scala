package io

import java.io.File

import config.Config
import model.{TrainedNerveNet, UnTrainNerveNet}

/**
  * Created by sparr on 2017/5/12.
  */
class Handler(nerveNetIO: NerveNetIO) {

  def show(savedFile:File): Unit = {
    nerveNetIO.load(savedFile)
    nerveNetIO.currentNerveNet match {
      case t:TrainedNerveNet[_] =>
        t.print()
    }
  }

  def save(saveFile:File): Unit = {
    nerveNetIO.save(saveFile)
  }

  def train(topFile:File,trainFile:File) : Unit = {
    nerveNetIO.loadTop(topFile)
    nerveNetIO.loadTrainCase(trainFile)
    val nerve = nerveNetIO.currentNerveNet
    nerve match {
      case unNerve:UnTrainNerveNet[_] =>
        unNerve.train()
      case _:TrainedNerveNet[_] => ()
    }
  }

  def predict(preFile:File): Unit ={
    val nerve = nerveNetIO.currentNerveNet
    nerveNetIO.loadPredictCase(preFile)
    nerve match {
      case unNerve:UnTrainNerveNet[_] =>
        for(input <- nerveNetIO.predictCase)
        {
          val trainedNerveNet = unNerve.toTrainedNerveNet
          val r = trainedNerveNet.predict(input)
          println(input+" :  "+ trainedNerveNet.getLabelName(r))
        }
      case trainNerve:TrainedNerveNet[_] =>
        for(input <- nerveNetIO.predictCase)
        {
          val r = trainNerve.predict(input)
          println(input+" :  "+ trainNerve.getLabelName(r))
        }
    }
  }

  def predict(input: Array[Double]): Unit ={
    nerveNetIO.currentNerveNet match {
      case unNerve:UnTrainNerveNet[_] =>
        val trainedNerveNet = unNerve.toTrainedNerveNet
        val r = trainedNerveNet.predict(input)
        println(trainedNerveNet.getLabelName(r))
      case trainNerve:TrainedNerveNet[_] =>
        val r = trainNerve.predict(input)
        println(trainNerve.getLabelName(r))
    }
  }


  def retrain(nerveFile:File,
              trainFile:File,
              savedFile:File,
              accuracy:Double): Unit = {
    nerveNetIO.retrain(nerveFile,savedFile,trainFile,accuracy)
    nerveNetIO.save(savedFile)
  }

  def test(savedFile: File,testFile:File): String = {
    nerveNetIO.load(savedFile)
    nerveNetIO.loadTestCase(testFile)
    var rate = ""
    nerveNetIO.currentNerveNet match {
      case t:TrainedNerveNet[_] =>
        rate = t.test(nerveNetIO.testInput.toArray,
          nerveNetIO.testExpClass.toArray)
//        println(rate)
    }
    rate
  }
}
