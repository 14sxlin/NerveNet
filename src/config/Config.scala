package config

import java.io.{File, FileInputStream, FileOutputStream, InputStreamReader}
import java.util.Properties

/**
  * 加载和修改配置文件
  */
object Config {

  private[this] var file:File = _

  def load(file: File,encoding:String="utf-8"): Properties = {
    this.file = file
    val properties = new Properties()
    properties.load(new InputStreamReader(new FileInputStream(file),encoding))
    properties
  }

  def store(properties: Properties): Unit ={
    properties.store(new FileOutputStream(file),"")
  }

}
