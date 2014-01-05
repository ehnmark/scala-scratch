package com.github.ehnmark.scratch

import Validator.{Failure, Success, Country}
import org.scalatest.FunSpec

class ValidatorSpec extends FunSpec {
	describe("A validator") {
		it("should return the first failure") {
			assert(Failure("minor")==Validator.test(10, "invalid", new Country("JP")))
			assert(Failure("missing @")==Validator.test(21, "invalid", new Country("JP")))
			assert(Failure("not available in Japan")==Validator.test(21, "a@b.c", new Country("JP")))
		}
		it("should return successfully if validation passes") {
			assert(Success("all passed")==Validator.test(21, "a@b.c", new Country("SE")))
		}
	}
}