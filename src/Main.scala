import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException

import config.Config
import io.NerveNetIO
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import collection.JavaConversions._
import scala.io.StdIn
/**
  * Created by sparr on 2017/5/13.
  */
object Main extends App{

  @Option(name="-p",usage = "指定配置文件的路径",required = true)
  var configPath = ""

  var topPath = ""
  var trainPath = ""
  var savedPath = ""
  var predictPath = ""
  var testPath = ""

  def init(): Unit = {
    val properties = Config.load(new File(configPath))
    topPath = properties.getProperty("topPath")
    trainPath = properties.getProperty("trainPath")
    savedPath = properties.getProperty("savedPath")
    predictPath = properties.getProperty("predictPath")
    testPath = properties.getProperty("testPath")
    println("configPath = "+configPath)
    println("topPath = "+topPath)
    println("trainPath = "+trainPath)
    println("savedPath = "+savedPath)
    println("predictPath = "+predictPath)
    println("testPath = "+testPath)
    println("load success...\n\n\n")
  }

  def printGuide(): Unit ={
    println("1.训练网络")
    println("2.评估网络")
    println("3.查看网络")
    println("4.预测")
    println("5.网络再训练")
    println("q.退出")
    println("请输入功能选项:")
  }


  val cmdLineParser = new CmdLineParser(this)
  try {
    assert(cmdLineParser != null)
    cmdLineParser.parseArgument(args.toIterable)
    init()

    val handler = new io.Handler(new NerveNetIO)
    printGuide()
    var input = StdIn.readLine()
    while(input!="q"){
      input match {
        case "1" => //训练
          val topFile = new File(topPath)
          val trainFile = new File(trainPath)
          val savedFile = new File(savedPath)
          if(!trainFile.exists())
            throw new FileNotFoundException("topFile not found")
          if(!trainFile.exists())
            throw new FileNotFoundException("trainFile not found")
          if(!savedFile.exists())
            savedFile.createNewFile()
          handler.train(topFile,trainFile)
          handler.save(savedFile)
          println("训练完成,保存到: "+savedPath)
          StdIn.readLine("回车继续")
          printGuide()
          input = StdIn.readLine()
        case "2" => //评估
          val testFile = new File(testPath)
          val savedFile = new File(savedPath)
          if(!testFile.exists())
            throw new FileNotFoundException("testFile 找不到")
          if(!savedFile.exists())
            throw new FileNotFoundException("savedFile 找不到")
          println(handler.test(savedFile,testFile))
          StdIn.readLine("回车继续")
          printGuide()
          input = StdIn.readLine()
        case "3" => //查看网络
          val savedFile = new File(savedPath)
          if(!savedFile.exists())
            throw new FileNotFoundException("savedFile 找不到")
          handler.show(savedFile)
          StdIn.readLine("回车继续")
          printGuide()
          input = StdIn.readLine()
        case "4" => // 预测
          if(predictPath.trim == ""){
            val line = StdIn.readLine("输入数据(空格分隔):")
            try {
              handler.predict(line.trim.split(" ").map(_.toDouble))

            }catch {
              case _:NumberFormatException =>
                printGuide()
                input = StdIn.readLine()
            }
          }else{
            val preFile = new File(predictPath)
            if(!preFile.exists())
              throw new FileNotFoundException()
            handler.predict(preFile)
            StdIn.readLine("回车继续")
            printGuide()
            input = StdIn.readLine()
          }
        case "5" => //再训练
          println("输入结果误差:")
          val accuracy = StdIn.readDouble()
          handler.retrain(
            new File(topPath),
            new File(trainPath),
            new File(savedPath),
            accuracy
          )
          println("再训练完成")
          StdIn.readLine("回车继续")
          printGuide()
          input = StdIn.readLine()
        case _ =>
          printGuide()
          input = StdIn.readLine()
      }

    }

  }catch {
    case e:CmdLineException =>
      println(e.getMessage)
      cmdLineParser.printUsage(System.err)
    case e:FileNotFoundException =>
      println("请确认文件路径正确")
      e.printStackTrace(System.out)
    case e:MalformedInputException =>
      println("文件的编码错误")
      e.printStackTrace(System.out)
  }


}
