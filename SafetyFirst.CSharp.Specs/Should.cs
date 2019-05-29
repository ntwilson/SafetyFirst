using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace SafetyFirst.CSharp.Specs {
  public static class ShouldExtensions {
    public static void ShouldBe<T>(this T actual, T expected) {
      Assert.AreEqual(expected, actual);
    }

    public static void ShouldNotBe<T>(this T actual, T expected) {
      Assert.AreNotEqual(expected, actual);
    }

    public static void ShouldSatisfy<T>(this T actual, Func<T, bool> predicate) {
      Assert.IsTrue(predicate(actual));
    }
  }
}