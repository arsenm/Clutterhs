-- -*-haskell-*-
--  GTypes for Clutter
--
--  Author : Matthew Arsenault
--
--  Created: 21 Nov 2009
--
--  Copyright (C) 2009 Matthew Arsenault
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 3 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

-- | All GTypes defined in Clutter headers
module Graphics.UI.Clutter.GTypes (
  actor,
  actorbox,
  alpha,
  animation,
  animatable,
  backend,
  behaviour,
  behaviourellipse,
  behaviourdepth,
  behaviouropacity,
  behaviourpath,
  behaviourrotate,
  behaviourscale,
  bindingpool,
  cairotexture,
  childmeta,
  clone,
  color,
  container,
  event,
  fog,
  geometry,
  gravity,
  group,
  interval,
  knot,
  listmodel,
  media,
  model,
  modeliter,
  paramcolor,
  paramunits,
  path,
  pathnode,
  perspective,
  rectangle,
  requestmode,
  score,
  script,
  scriptable,
  shader,
  shaderfloat,
  shaderint,
  shadermatrix,
  stage,
  stagemanager,
  text,
  texture,
  timeline,
  units,
  vertex,

#if CLUTTER_CHECK_VERSION(1,2,0)
  binLayout,
  flowOrientation
#endif
  ) where

import C2HS
import System.Glib.GType

{# fun pure unsafe actor_get_type as actor { } -> `GType' cToEnum #}
{# fun pure unsafe actor_box_get_type as actorbox { } -> `GType' cToEnum #}
{# fun pure unsafe alpha_get_type as alpha { } -> `GType' cToEnum #}
{# fun pure unsafe animation_get_type as animation { } -> `GType' cToEnum #}
{# fun pure unsafe animatable_get_type as animatable { } -> `GType' cToEnum #}
{# fun pure unsafe backend_get_type as backend { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_get_type as behaviour { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_ellipse_get_type as behaviourellipse { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_depth_get_type as behaviourdepth { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_opacity_get_type as behaviouropacity { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_path_get_type as behaviourpath { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_rotate_get_type as behaviourrotate { } -> `GType' cToEnum #}
{# fun pure unsafe behaviour_scale_get_type as behaviourscale { } -> `GType' cToEnum #}
{# fun pure unsafe binding_pool_get_type as bindingpool { } -> `GType' cToEnum #}
{# fun pure unsafe cairo_texture_get_type as cairotexture { } -> `GType' cToEnum #}
{# fun pure unsafe child_meta_get_type as childmeta { } -> `GType' cToEnum #}
{# fun pure unsafe clone_get_type as clone { } -> `GType' cToEnum #}
{# fun pure unsafe color_get_type as color { } -> `GType' cToEnum #}
{# fun pure unsafe container_get_type as container { } -> `GType' cToEnum #}
{# fun pure unsafe event_get_type as event { } -> `GType' cToEnum #}
{# fun pure unsafe fog_get_type as fog { } -> `GType' cToEnum #}
{# fun pure unsafe geometry_get_type as geometry { } -> `GType' cToEnum #}
{# fun pure unsafe gravity_get_type as gravity { } -> `GType' cToEnum #}
{# fun pure unsafe group_get_type as group { } -> `GType' cToEnum #}
{# fun pure unsafe interval_get_type as interval { } -> `GType' cToEnum #}
{# fun pure unsafe knot_get_type as knot { } -> `GType' cToEnum #}
{# fun pure unsafe list_model_get_type as listmodel { } -> `GType' cToEnum #}
{# fun pure unsafe media_get_type as media { } -> `GType' cToEnum #}
{# fun pure unsafe model_get_type as model { } -> `GType' cToEnum #}
{# fun pure unsafe model_iter_get_type as modeliter { } -> `GType' cToEnum #}
{# fun pure unsafe param_color_get_type as paramcolor { } -> `GType' cToEnum #}
{# fun pure unsafe param_units_get_type as paramunits { } -> `GType' cToEnum #}
{# fun pure unsafe path_get_type as path { } -> `GType' cToEnum #}
{# fun pure unsafe path_node_get_type as pathnode { } -> `GType' cToEnum #}
{# fun pure unsafe perspective_get_type as perspective { } -> `GType' cToEnum #}
{# fun pure unsafe rectangle_get_type as rectangle { } -> `GType' cToEnum #}
{# fun pure unsafe request_mode_get_type as requestmode { } -> `GType' cToEnum #}
{# fun pure unsafe score_get_type as score { } -> `GType' cToEnum #}
{# fun pure unsafe script_get_type as script { } -> `GType' cToEnum #}
{# fun pure unsafe scriptable_get_type as scriptable { } -> `GType' cToEnum #}
{# fun pure unsafe shader_get_type as shader { } -> `GType' cToEnum #}
{# fun pure unsafe shader_float_get_type as shaderfloat { } -> `GType' cToEnum #}
{# fun pure unsafe shader_int_get_type as shaderint { } -> `GType' cToEnum #}
{# fun pure unsafe shader_matrix_get_type as shadermatrix { } -> `GType' cToEnum #}
{# fun pure unsafe stage_get_type as stage { } -> `GType' cToEnum #}
{# fun pure unsafe stage_manager_get_type as stagemanager { } -> `GType' cToEnum #}
{# fun pure unsafe text_get_type as text { } -> `GType' cToEnum #}
{# fun pure unsafe texture_get_type as texture { } -> `GType' cToEnum #}
{# fun pure unsafe timeline_get_type as timeline { } -> `GType' cToEnum #}
{# fun pure unsafe units_get_type as units { } -> `GType' cToEnum #}
{# fun pure unsafe vertex_get_type as vertex { } -> `GType' cToEnum #}

#if CLUTTER_CHECK_VERSION(1,2,0)

{# fun pure unsafe bin_layout_get_type as binLayout { } -> `GType' cToEnum #}
{# fun pure unsafe flow_orientation_get_type as flowOrientation { } -> `GType' cToEnum #}

#endif

