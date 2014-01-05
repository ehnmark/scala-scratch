package com.github.ehnmark.scratch

object Validator {
	sealed trait Result[+A] {
		def flatMap[B](f: A => Result[B]): Result[B]
		def map[B](f: A => B): Result[B]
	}
	case class Success[A](value: A) extends Result[A] {
		override def flatMap[B](f: A => Result[B]) = f(value)
		override def map[B](f: A => B) = new Success(f(value))
	}

	case class Failure[A](msg: String) extends Result[A] {
		override def flatMap[B](f: A => Result[B]) = new Failure[B](msg)
		override def map[B](f: A => B) = new Failure[B](msg)
	}

	case class Country(code: String)
	def test(inAge: Int, inEmail: String, inCountry: Country) = {
		def validateAge(age: Int) = if(age > 20) Success(age) else Failure("minor")
		def validateEmail(address: String) =
			if (address.contains('@')) Success(address)
			else Failure("missing @")
		def validateCountry(country: Country) = country match {
			case Country("JP") => Failure("not available in Japan")
			case x => Success(x)
		}
		for {
			a <- validateAge(inAge)
			e <- validateEmail(inEmail)
			c <- validateCountry(inCountry)
		} yield "all passed"
	}

}
