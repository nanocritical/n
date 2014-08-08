// Copyright 2011, Nanocritical Corp. and the nntools Project contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Hash functions.

#ifndef HASH_H__
#define HASH_H__

#include <stdint.h>
#include <stdlib.h>

#define hash32 hash32_hsieh
uint32_t hash32_hsieh(const void *data, size_t len);

uint32_t hash32_n(const void *data, size_t len);

#endif
