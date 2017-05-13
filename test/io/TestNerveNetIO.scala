package io

import java.io.File

import model.{TrainedNerveNet, UnTrainNerveNet}
import org.scalatest.FunSuite

/**
  * Created by sparr on 2017/5/1.
  */
class TestNerveNetIO extends FunSuite{

  val nerveNetIO = new NerveNetIO()

  test("test train"){
//    val nerveFile = new File("resource/nervenode_flower.input")
//    val trainFile = new File("resource/train_flower.txt")
//    val nerveFile = new File("resource/nervenode_test.input")
//    val trainFile = new File("resource/train_test.txt")
    val nerveFile = new File("resource/nervenode_exclusive_or.input")
    val trainFile = new File("resource/train_exclusive_or.txt")
    nerveNetIO.loadTop(nerveFile)
    nerveNetIO.loadTrainCase(trainFile)
    val unTrainNerveNet =
      nerveNetIO.currentNerveNet.asInstanceOf[UnTrainNerveNet[String]]
//    nerveNetAlgorithm.printRealOutput()
    unTrainNerveNet.train()
//    nerveNetAlgorithm.printRealOutput()
    unTrainNerveNet.printWeight()
    unTrainNerveNet.printXita()
    unTrainNerveNet.printErr()
    val savedFile = new File("resource/saved_exclusive_or.out")
//    val savedFile = new File("resource/saved_flower.out")
    nerveNetIO.save(savedFile)
  }


  test("test reload trained model"){
    val savedFile = new File("resource/saved_exclusive_or.out")
    nerveNetIO.load(savedFile)
    val nerve = nerveNetIO.currentNerveNet
    nerve.printRealOutput()
    nerve match {
      case tNerve:TrainedNerveNet[String] =>
        nerve.printWeight()
        nerve.printXita()
        val r1 = tNerve.predict(Array(1,0))
        println("1 :"+tNerve.getLabelName(r1))
        val r2 = tNerve.predict(Array(0,1))
        println("1 :"+tNerve.getLabelName(r2))
        val r3 = tNerve.predict(Array(0,0))
        println("0 :"+tNerve.getLabelName(r3))
        val r4 = tNerve.predict(Array(1,1))
        println("0 :"+tNerve.getLabelName(r4))
    }
  }

  test("continue train"){//4 + 100    结束在 43425
    var i = 0
    val nerveFile = new File("resource/nervenode_flower.input")
    val trainFile = new File("resource/train_flower.txt")
    val savedFile = new File("resource/saved_flower.out")

    while(i<100)
    {
      nerveNetIO.retrain(nerveFile,savedFile,trainFile,0.00001)
      nerveNetIO.save(savedFile)
      i += 1
    }
    println("i = "+i + " * 100000")
  }

  test("test rate"){
    val testFile = new File("resource/test_flower.txt")
    val savedFile = new File("resource/saved_flower.out")
    nerveNetIO.load(savedFile)
    nerveNetIO.loadTestCase(testFile)
    nerveNetIO.currentNerveNet match {
      case t:TrainedNerveNet[_] =>
        val rate = t.test(nerveNetIO.testInput.toArray,
          nerveNetIO.testExpClass.toArray)
        println(rate)
    }
  }
}
