package model

import scala.collection.mutable

/**
  * Created by sparr on 2017/5/12.
  */
@SerialVersionUID(512)
class NerveTop[T] extends Serializable{

  /**
    * 神经网络的名称
    * 0层是输入层
    * 最后一层是输出层
    * 其他的是中间层
    */
  var nerveNode : Array[Array[T]] = _

  /**
    * 权重 weight((i,j))表示神经元i到j的权重
    *  其中i,j是节点的名称
    */
  var weight : mutable.Map[(T,T),Double] = _

  /**
    * 神经单元的阈值
    * xita(i) 表示神经元名称为i的阈值
    */
  var xita :mutable.Map[T,Double] = _

}
