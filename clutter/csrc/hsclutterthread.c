/* Mostly taken from gtk2hs for gtk threads. Clutter has its own
 * locking, so basically the same thing using equivalent clutter
 * functions. TODO: It turns out the only difference seems to be using
 * clutter_threads_init instead of gdk_threads_init, so maybe only
 * keep that */

#include <glib.h>
#include <glib/gthread.h>
#include <clutter/clutter.h>

#undef DEBUG

static GStaticMutex clutterhs_finalizer_mutex;
static GSource* clutterhs_finalizer_source;
static guint clutterhs_finalizer_id;
static GArray* clutterhs_finalizers;

gboolean clutterhs_run_finalizers(gpointer data);

/* Initialize the threads system of Clutter. */
void clutterhs_threads_initialise () {
    static int threads_initialised = 0;

    if (!threads_initialised)
    {
        threads_initialised = 1;
        g_thread_init(NULL);
        clutter_threads_init();
    }
}

/* Free an object within the Clutterhs lock. */
void clutterhs_g_object_unref_from_mainloop(gpointer object) {
    g_static_mutex_lock(&clutterhs_finalizer_mutex);

#ifdef DEBUG
    printf("adding finalizer!\n");
#endif

    /* Ensure that the idle handler is still installed and that
       the array of objects that are to be finalized exists. */
    if (clutterhs_finalizer_id == 0)
    {

        if (clutterhs_finalizers == NULL)
            clutterhs_finalizers = g_array_new(0, 0, sizeof(gpointer));

        if (clutterhs_finalizer_source != NULL)
        {
            g_source_destroy(clutterhs_finalizer_source);
            g_source_unref(clutterhs_finalizer_source);
        }

        clutterhs_finalizer_source = g_idle_source_new();
        g_source_set_callback(clutterhs_finalizer_source, &clutterhs_run_finalizers, NULL, NULL);
        clutterhs_finalizer_id = g_source_attach(clutterhs_finalizer_source, NULL);

    }

    /* Add the object to the list. */
    g_array_append_val(clutterhs_finalizers, object);

    g_static_mutex_unlock(&clutterhs_finalizer_mutex);
}

/* Run the finalizers that have been accumulated. */
gboolean clutterhs_run_finalizers(gpointer data) {
    gint index;
    g_assert(clutterhs_finalizers != NULL);

    g_static_mutex_lock(&clutterhs_finalizer_mutex);

#ifdef DEBUG
    printf("running %i finalizers!\n", clutterhs_finalizers->len);
#endif

    for (index = 0; index < clutterhs_finalizers->len; index++)
        g_object_unref(g_array_index (clutterhs_finalizers, GObject*, index));

    g_array_set_size(clutterhs_finalizers, 0);

    clutterhs_finalizer_id = 0;

    g_static_mutex_unlock(&clutterhs_finalizer_mutex);

    return FALSE;
}

