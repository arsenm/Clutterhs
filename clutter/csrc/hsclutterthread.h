#include <glib.h>

#ifndef HSCLUTTERTHREAD_H
#define HSCLUTTERTHREAD_H

/* Initialize the threads for clutter. */
int clutterhs_threads_initialise (void);

/* Free an object within the Clutter main loop. */
void clutterhs_g_object_unref_from_mainloop(gpointer object);

#endif HSCLUTTERTHREAD_H

