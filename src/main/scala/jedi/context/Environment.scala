package jedi.context

import jedi.value.Value
import jedi.expression.Identifier

class Environment(var extension: Environment = null) extends collection.mutable.HashMap[Identifier, Value] with Value:

  // used by closures to bind parameters to arguments
  def bulkPut(params: List[Identifier], args: List[Value]): Unit =
    if (params.length != args.length) throw TypeException("# arguments != #parameters")
    for(i <- params.indices) this.put(params(i), args(i))


  override def contains(name: Identifier): Boolean =
    super.contains(name) || (extension != null && extension.contains(name))


  override def apply(name: Identifier): Value =
    if (super.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw UndefinedException(name)
