#ifndef CLUTTER_MACROS_H_
#define CLUTTER_MACROS_H_
/* CPP macros */

#include <clutter/clutter.h>

gboolean actor_is_realized(ClutterActor* actor);
gboolean actor_is_visible(ClutterActor* actor);
gboolean actor_is_reactive(ClutterActor* actor);
gboolean actor_is_mapped(ClutterActor* actor);

#endif

