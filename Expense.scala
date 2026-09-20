case class Expense(category: String, amount: BigDecimal)
@main def expenseReport(values: String*): Unit =
  val expenses = values.flatMap(_.split(":") match { case Array(category, amount) => amount.toBigDecimalOption.map(Expense(category, _)); case _ => None })
  if expenses.isEmpty then println("Uso: Categoria:Importe ...")
  else
    expenses.groupMapReduce(_.category)(_.amount)(_ + _).toSeq.sortBy(-_._2).foreach((category, total) => println(f"$category%-20s $$${total}%.2f"))
    println(f"Total: $$${expenses.map(_.amount).sum}%.2f")
