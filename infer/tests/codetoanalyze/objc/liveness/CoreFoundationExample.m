/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

static NSDictionary* dictionaryRepresentationFromCFPreferences(
    NSString* preferencesID) {
  CFStringRef ID = (__bridge CFStringRef)preferencesID;
  return (__bridge_transfer NSDictionary*)CFPreferencesCopyMultiple(
      NULL, ID, kCFPreferencesCurrentUser, kCFPreferencesAnyHost);
}
