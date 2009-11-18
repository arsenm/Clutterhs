
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

