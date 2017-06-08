/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

//package codetoanalyze.java.quandary;

import java.util.Formatter;

import com.facebook.infer.builtins.InferTaint;

/** a lot of tainted values are strings, so propagation through StringBuilder's and the like is very
  * important. */

public class Strings {

  void viaStringBuilderSugarBad() {
    Object source = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(source + "");
  }

  void viaStringBuilderBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuilder builder = new StringBuilder();
    InferTaint.inferSensitiveSink(builder.append(source).append("").toString());
  }

  void viaStringBuilderIgnoreReturnBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuilder builder = new StringBuilder();
    // builder should be tainted after this call even though we ignore the return value
    builder.append(source);
    InferTaint.inferSensitiveSink(builder.toString());
  }

  void viaStringBufferBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuffer buffer = new StringBuffer();
    InferTaint.inferSensitiveSink(buffer.append("").append(source).toString());
  }

  void viaStringBufferIgnoreReturnBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuffer buffer = new StringBuffer();
    buffer.append(source);
    InferTaint.inferSensitiveSink(buffer.toString());
  }

  void viaFormatterBad() {
    Object source = InferTaint.inferSecretSource();
    Formatter formatter = new Formatter();
    InferTaint.inferSensitiveSink(formatter.format("%s", source).toString());
  }

  void viaFormatterIgnoreReturnBad() {
    Object source = InferTaint.inferSecretSource();
    Formatter formatter = new Formatter();
    formatter.format("%s", source);
    InferTaint.inferSensitiveSink(formatter.toString());
  }

  void viaStringFormatVarArgsDirectBad() {
    Object source = InferTaint.inferSecretSource();
    String tainted = String.format("%s%s", "hi", source);
    InferTaint.inferSensitiveSink(tainted);
  }

  void viaStringFormatVarArgsIndirect(Object param) {
    String tainted = String.format("%s%s", "hi", param);
    InferTaint.inferSensitiveSink(tainted);
  }

  void viaStringFormatVarArgsIndirectBad() {
    viaStringFormatVarArgsIndirect(InferTaint.inferSecretSource());
  }

}
