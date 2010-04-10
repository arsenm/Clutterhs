/*  GIMP Toolkit (GTK) HSGClosure interface
 *
 *  Author : Duncan Coutts
 *
 *  Created: 22 March 2005
 *
 *  Copyright (C) 2005 Duncan Coutts
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 */

#include <glib-object.h>
#include <Rts.h>

#ifndef HSGVALUE_H_
#define HSGVALUE_H_

HsStablePtr g_value_to_haskellobj(const GValue *value);
void g_value_from_haskellobj(GValue *value, HsStablePtr hsVal);

#endif /* HSGVALUE_H_ */

