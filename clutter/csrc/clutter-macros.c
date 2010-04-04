
#include <clutter/clutter.h>
#include <clutter-macros.h>

gboolean actor_is_realized(ClutterActor* actor)
{
    return CLUTTER_ACTOR_IS_REALIZED(actor);
}

gboolean actor_is_visible(ClutterActor* actor)
{
    return CLUTTER_ACTOR_IS_VISIBLE(actor);
}

gboolean actor_is_reactive(ClutterActor* actor)
{
    return CLUTTER_ACTOR_IS_REACTIVE(actor);
}

gboolean actor_is_mapped(ClutterActor* actor)
{
    return CLUTTER_ACTOR_IS_MAPPED(actor);
}

#if CLUTTER_CHECK_VERSION(1,2,0)

guint clutterhs_clutter_major_version()
{
    return clutter_major_version;
}

guint clutterhs_clutter_minor_version()
{
    return clutter_minor_version;
}

guint clutterhs_clutter_micro_version()
{
    return clutter_micro_version;
}

#endif

