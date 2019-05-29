using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using SafetyFirst.CSharp;
using static SafetyFirst.Result;
using SafetyFirst;
using Microsoft.FSharp.Core;

namespace SafetyFirst.CSharp.Specs {
  [TestClass]
  public class ResultSpec {

    [TestMethod]
    public void MatchMemberShouldReturnAnAppropriateValue() {
      Ok<int, string>(5).Match(
        ok: i => i.ToString(),
        error: err => err)
      .ShouldBe("5");
      
      Error<int, string>("didn't work").Match(
        ok: i => i.ToString(),
        error: err => err)
      .ShouldBe("didn't work");
    }

    [TestMethod]
    public void MatchMemberShouldSupportActions() {
      var didRun = false;
      Ok<int, string>(5).Match(
        ok: i => { didRun = true; },
        error: err => { });

      didRun.ShouldBe(true);

      didRun = false;

      Error<int, string>("didn't work").Match(
        ok: i => {},
        error: err => { didRun = true; });

      didRun.ShouldBe(true);
    }

    [TestMethod]
    public void HasMembersToExecuteActionsSpecificallyForErrorsOrOkes() {
      var didRun = false;
      Ok<int, string>(5).IfOk(i => { didRun = true; });
      didRun.ShouldBe(true);

      didRun = false;
      Error<int, string>("didn't work").IfOk(i => { didRun = true; });
      didRun.ShouldBe(false);

      didRun = false;
      Error<int, string>("didn't work").IfError(err => { didRun = true; });
      didRun.ShouldBe(true);

      didRun = false;
      Ok<int, string>(5).IfError(err => { didRun = true; });
      didRun.ShouldBe(false);
    }

    [TestMethod]
    public void MapMemberShouldWorkWithNonResults() {
      Ok<int, string>(5).Map(i => i + 3)
      .ShouldBe(Ok<int, string>(8));

      Error<int, string>("didn't work").Map(i => i + 3)
      .ShouldBe(Error<int, string>("didn't work"));
    }

    [TestMethod]
    public void MapErrorMemberShouldTransformErrorsButLeaveOkResultsTheSame() { 
      Error<string, int>(5).MapError(i => i + 3)
      .ShouldBe(Error<string, int>(8));

      Ok<string, int>("didn't work").MapError(i => i + 3)
      .ShouldBe(Ok<string, int>("didn't work"));
    }

    [TestMethod]
    public void BindMemberShouldWorkWithResultFuncs() {
      Ok<int, string>(5).Bind(i => Ok<int, string>(i + 3))
      .ShouldBe(Ok<int,string>(8));

      Error<int, string>("didn't work").Bind(i => Ok<int, string>(i + 3))
      .ShouldBe(Error<int, string>("didn't work"));
    }

    [TestMethod]
    public void StaticBindAndMapMembersPassThroughToFSharpVersions() {
      Result.Map2((int a, string b) => a.ToString() + b,
        Ok<int, string>(99),
        Ok<string, string>(" bottles of beer on the wall"))
      .ShouldBe(Ok<string, string>("99 bottles of beer on the wall"));

      Result.Bind3(
        (int a, string b, string c) => 
          Ok<string, string>(a.ToString() + b + ". " + a.ToString() + c),
        Ok<int, string>(99),
        Ok<string, string>(" bottles of beer on the wall"),
        Ok<string, string>(" bottles of beer"))
      .ShouldBe(Ok<string, string>("99 bottles of beer on the wall. 99 bottles of beer"));
    }

    [TestMethod]
    public void CanUseADefaultValueForAFailedResult() {
      Error<int, string>("didn't work").OkOrElse(10).ShouldBe(10);
      Ok<string, string>("yay").OkOrElse("it didn't work").ShouldBe("yay");

      Error<int, string>("didn't work").OkOrElse(() => 10).ShouldBe(10);
      Ok<string, string>("yay").OkOrElse(() => "it didn't work").ShouldBe("yay");
    }

    [TestMethod]
    public void CanConvertToAndFromAnFSharpResult() {
      Error<int, string>("didn't work").ToFs().ShouldBe(FSharpResult<int, string>.NewError("didn't work"));
      Ok<int, string>(10).ToFs().ShouldBe(FSharpResult<int, string>.NewOk(10));

      Result.FromFs(Error<int, string>("didn't work").ToFs()).ShouldBe(Error<int, string>("didn't work"));
      Result.FromFs(Ok<int, string>(10).ToFs()).ShouldBe(Ok<int, string>(10));
    }

    [TestMethod]
    public void CanUseResultsInALinqExpression() {
      (from a in Ok<int, string>(99)
       from b in Ok<string, string>(" bottles of beer on the wall")
       from c in Ok<string, string>(" bottles of beer")
       select a.ToString() + b + ". " + a.ToString() + c
      ).ShouldBe(Ok<string, string>("99 bottles of beer on the wall. 99 bottles of beer"));

      (from a in Ok<int, string>(99)
       from b in Ok<string, string>(" bottles of beer on the wall")
       from c in Error<string, string>("Dropped one!")
       select a.ToString() + b + ". " + a.ToString() + c
      ).ShouldBe(Error<string, string>("Dropped one!"));
    }

    Exception expectException(Action act) {
      try {
        act();
      }
      catch (Exception ex) {
        return ex;
      }
      throw new AssertFailedException("Expected an Exception, but none was thrown"); 
    }

    [TestMethod]
    public void UnlessCanExtractTheValueFromAResultWithAMessage() {
      Ok<int, string>(99).Unless("Expected to be able to extract a value from an Ok Result")
        .ShouldBe(99);
      
      var ex = expectException(() => Error<int, string>("Didn't work").Unless("Can't find"));
        
      ex.Message.ShouldSatisfy(it => it.Contains("Didn't work") && it.Contains("Can't find"));
      
      if (ex is ResultExpectedException<string> rex) rex.ErrorDetails.ShouldBe("Didn't work");
      else throw new AssertFailedException($"Expected a ResultExpectedException, but got a {ex.GetType().FullName}.");
    }

    [TestMethod]
    public void ExpectCanExtractTheValueFromAResultWithoutAMessage() {
      Ok<int, string>(99).Expect()
        .ShouldBe(99);
      
      var ex = expectException(() => Error<int, string>("Didn't work").Expect());
        
      ex.Message.ShouldSatisfy(it => it.Contains("Didn't work"));
      
      if (ex is ResultExpectedException<string> rex) rex.ErrorDetails.ShouldBe("Didn't work");
      else throw new AssertFailedException($"Expected a ResultExpectedException, but got a {ex.GetType().FullName}.");
    }

    [TestMethod]
    public void FSharpResultsHaveAnExtensionMethodToConvertToTheCSharpResultType() { 
      FSharpResult<int, string>.NewOk(5).ToCs().ShouldSatisfy(it => it is Result<int, string>);
    }
    
    class PretendError {}

    [TestMethod]
    public void IsEasyToAddContextToResultErrors() {
      Result<int, PretendError> x = Error<int, PretendError>(new PretendError());
      Result<int, ErrorWithContext<PretendError>> y = x.WithContext("this error is totally pretend");
      Result<int, ErrorWithContext<PretendError>> z = y.WithContext("really, really pretend");

      z.Match(
        error: e => e.Context.ShouldSatisfy(xs => xs[0] == "really, really pretend" && xs[1] == "this error is totally pretend"),
        ok: _ => throw new AssertFailedException("should be error")
      );
    } 
  }
}
