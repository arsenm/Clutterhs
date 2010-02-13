
/* I do this since CoglTextureVertex has a CoglColor struct within
 * (not a pointer to it), which is opaque. */

#include <cogl/cogl.h>
#include <string.h>

#include "cogl_workarounds.h"

CoglColor* coglTextureVertexColorAccess( CoglTextureVertex* v )
{
    return &(v->color);
}

void coglTextureVertexColorWrite( CoglTextureVertex* v, CoglColor* c )
{
    memcpy(&(v->color), c, sizeof(CoglColor));
}

