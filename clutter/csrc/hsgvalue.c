/*  Experiment to deal with GValues differently
 *
 *  Author : Duncan Coutts
 *
 *  Created: 22 March 2005
 *  Modified: 10 April 2010
 *
 *  Copyright (C) 2005 Duncan Coutts
 *  Copyright (C) 2010 Matthew Arsenault
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

/* GHC's semi-public Rts API */
#include <Rts.h>

#include "hsgvalue.h"

#ifdef DEBUG
#define WHEN_DEBUG(a) a
#else
#define WHEN_DEBUG(a)
#endif


/* forward defs */
static HaskellObj gtk2hs_value_as_haskellobj(Capability *cap, const GValue *value);
static void gtk2hs_value_from_haskellobj(GValue *value, HaskellObj obj);

/* GValue <-> HaskellObj marshaling functions */

inline static HaskellObj gtk2hs_value_as_haskellobj(Capability *cap, const GValue *value) {
    switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
    case G_TYPE_INTERFACE:
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
            return rts_mkPtr(cap, g_value_get_object(value));
        else
            break;
    case G_TYPE_CHAR:
        return rts_mkChar(cap, g_value_get_char(value));
    case G_TYPE_UCHAR:
        return rts_mkChar(cap, g_value_get_uchar(value));
    case G_TYPE_BOOLEAN:
        return rts_mkBool(cap, g_value_get_boolean(value));
    case G_TYPE_INT:
        return rts_mkInt(cap, g_value_get_int(value));
    case G_TYPE_UINT:
        return rts_mkWord(cap, g_value_get_uint(value));
    case G_TYPE_LONG:
        return rts_mkInt(cap, g_value_get_long(value));
    case G_TYPE_ULONG:
        return rts_mkWord(cap, g_value_get_ulong(value));
/*    case G_TYPE_INT64:
        return rts_mkInt64(cap, g_value_get_int64(value));
    case G_TYPE_UINT64:
        return rts_mkWord64(cap, g_value_get_uint64(value));   */
    case G_TYPE_ENUM:
        return rts_mkInt(cap, g_value_get_enum(value));
    case G_TYPE_FLAGS:
        return rts_mkWord(cap, g_value_get_enum(value));
    case G_TYPE_FLOAT:
        return rts_mkFloat(cap, g_value_get_float(value));
    case G_TYPE_DOUBLE:
        return rts_mkDouble(cap, g_value_get_double(value));
    case G_TYPE_STRING:
        return rts_mkPtr(cap, (char *)g_value_get_string(value)); /* CHECKME: is the string freed? */
    case G_TYPE_POINTER:
        return rts_mkPtr(cap, g_value_get_pointer(value));
    case G_TYPE_BOXED:
        return rts_mkPtr(cap, g_value_get_boxed(value));
/*    case G_TYPE_PARAM:
        return g_value_get_param(value); */
    case G_TYPE_OBJECT:
        return rts_mkPtr(cap, g_value_get_object(value));
    }
    g_error("gtk2hs_value_as_haskellobj: unable to handle GValue with type %s\n"
            "please report this as a bug to arsenm2@rpi.edu",
            g_type_name(G_VALUE_TYPE(value)));
}

void gtk2hs_value_from_haskellobj(GValue *value, HaskellObj obj)
{

    switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
    case G_TYPE_INVALID:
    case G_TYPE_NONE:
        return;
    case G_TYPE_INTERFACE:
        /* we only handle interface types that have a GObject prereq */
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
        {
            g_value_set_object(value, rts_getPtr(obj));
        }
        else
        {
            break;
        }
        return;
    case G_TYPE_CHAR:
        g_value_set_char(value, rts_getChar(obj));
        return;
    case G_TYPE_UCHAR:
        g_value_set_char(value, rts_getChar(obj));
        return;
    case G_TYPE_BOOLEAN:
        g_value_set_boolean(value, rts_getBool(obj));
        return;
    case G_TYPE_INT:
        g_value_set_int(value, rts_getInt(obj));
        return;
    case G_TYPE_UINT:
        g_value_set_uint(value, rts_getWord(obj));
        return;
    case G_TYPE_LONG:
        g_value_set_long(value, rts_getInt(obj));
        return;
    case G_TYPE_ULONG:
        g_value_set_ulong(value, rts_getWord(obj));
        return;
/*    case G_TYPE_INT64:
        g_value_set_int64(value, rts_getInt64(obj));
        return;
    case G_TYPE_UINT64:
        g_value_set_uint64(value, rts_getWord64(obj));
        return;                                         */
    case G_TYPE_ENUM:
        g_value_set_enum(value, rts_getInt(obj));
        return;
    case G_TYPE_FLAGS:
        g_value_set_flags(value, rts_getInt(obj));
        return;
    case G_TYPE_FLOAT:
        g_value_set_float(value, rts_getFloat(obj));
        return;
    case G_TYPE_DOUBLE:
        g_value_set_double(value, rts_getDouble(obj));
        return;
    case G_TYPE_STRING:
        g_value_set_string(value, rts_getPtr(obj));
        return;
    case G_TYPE_POINTER:
        g_value_set_pointer(value, rts_getPtr(obj));
        return;
/*    case G_TYPE_BOXED: {
        g_value_set_boxed(value, obj);
        break;
    }
    case G_TYPE_PARAM:
        g_value_set_param(value, (obj));
        break;                                          */
    case G_TYPE_OBJECT:
        g_value_set_object(value, rts_getPtr(obj));
        return;
    }
    g_error("gtk2hs_value_from_haskellobj: unable to handle GValue with type %s\n"
            "please report this as a bug to arsenm2@rpi.edu",
            g_type_name(G_VALUE_TYPE(value)));
}


HsStablePtr g_value_to_haskellobj(const GValue *value)
{
    Capability *cap;
    HaskellObj obj;
    HsStablePtr ret;

    cap = rts_lock();
    obj = gtk2hs_value_as_haskellobj(cap, value);
    rts_unlock(cap);

    ret = (HsStablePtr) getStablePtr((StgPtr) obj);

    return ret;
}

void g_value_from_haskellobj(GValue *value, HsStablePtr hsVal)
{
    HaskellObj ret;
    StgPtr stableVal;
    Capability *cap;

    stableVal = (StgPtr) deRefStablePtr(hsVal);

    /* evaluate the value you're putting in the GValue */
    cap = rts_lock();

    WHEN_DEBUG(g_debug("g_value_from_haskellobj: about to rts_eval");)
    rts_eval(&cap, (HaskellObj) stableVal, &ret);
    WHEN_DEBUG(g_debug("g_value_from_haskellobj: rts_eval done");)

    rts_unlock(cap);

    /* die when things break */
    rts_checkSchedStatus("g_value_from_haskellobj", cap);

    /* put the evaluated value into the GValue */
    gtk2hs_value_from_haskellobj(value, ret);

}

