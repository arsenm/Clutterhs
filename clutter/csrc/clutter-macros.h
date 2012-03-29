#ifndef CLUTTER_MACROS_H_
#define CLUTTER_MACROS_H_
/* CPP macros */

#define X_H
#define _X11_XLIB_H_
#include <clutter/clutter.h>

gboolean actor_is_realized(ClutterActor* actor);
gboolean actor_is_visible(ClutterActor* actor);
gboolean actor_is_reactive(ClutterActor* actor);
gboolean actor_is_mapped(ClutterActor* actor);

#if CLUTTER_CHECK_VERSION(1,2,0)

guint clutterhs_clutter_major_version();
guint clutterhs_clutter_minor_version();
guint clutterhs_clutter_micro_version();

#endif  /* CLUTTER_CHECK_VERSION(1,2,0) */

#endif  /* CLUTTER_MACROS_H_ */

