#include <cairo.h>
#include <pango/pangocairo.h>

void doalloc() {
    cairo_surface_t *surface =
        cairo_image_surface_create(CAIRO_FORMAT_RGB24, 10, 10);
    cairo_t        *c = cairo_create(surface);
    PangoContext* pc = pango_cairo_create_context(c);
    PangoLayout *layout = pango_layout_new(pc);

    g_object_unref(layout);
    g_object_unref(pc);
    cairo_destroy(c);
    cairo_surface_destroy(surface);
}

int main(int argc, char **argv)
{
    for (int i = 0;; i++) {
        doalloc();
    }
}
