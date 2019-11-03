################################################################################
#
# Description:
#
# This module implements a simple ray tracer. Its features are:
#
#    - Multiple point (white) light sources.
#    - Multiple coloured objects (spheres, planes).
#    - Ambient illumination (simulating scattered light).
#    - Diffuse illumination (simulating light scattering surfaces).
#    - Specular illumination using the Phong model (simulating highlights).
#    - Reflective illumination (simulating mirrored surfaces).
#    - Shadows.
#
# It was written for the purposes of demonstrating Ray Tracing in an 
# Advanced Lecture of the University of Melbourne subject COMP10001
# Foundations of Computing. As such the emphasis is on simplicity and
# clarity over performance. There are many ways to make this program
# faster, but usually at the expence of code readability.
#
# Authors:
#
# Bernie Pope (bjpope@unimelb.edu.au)
#
# Date created:
#
# 17 August 2012
#
# Date modified and reason:
#
# 21 August 2012: more documentation added 
# 22 August 2012: plane objects added
# 23 August 2012: command line argument parser added
#
################################################################################

from math import sqrt, acos
import argparse

# Fudge factor to check intersects with line segments are at least this distance
# in front of the line start point. Without this we can get reflected rays
# intersecting with the originating object.
intersect_epsilon = 0.1

def write_image(image, filename):
    """write_image(image, filename) -> None

    Writes image data file to filename.

    Input image must be rectangular, grey-scale, 8 bits per pixel,
    in row major coordinates.
    """
    flat_pixels = []
    for row in image:
        flat_pixels += row
    print(flat_pixels)
    #out_image = Image.new('RGB', (get_width(image), get_height(image)))
    #out_image.putdata(flat_pixels)
    #out_image.save(filename)

def get_width(image):
    """get_width(image) -> integer width of the image (number of columns).

    Input image must be rectangular list of lists. The width is
    taken to be the length of the first row of pixels. If the image is
    empty, then the width is defined to be 0.
    """
    if len(image) == 0:
        return 0
    else:
        return len(image[0])

def get_height(image):
    """get_height(image) -> integer height of the image (number of rows).

    Input image must be rectangular list of lists. The height is
    taken to be the number of rows.
    """
    return len(image)

# We also represent coordinates in 3D space as Vectors.
class Vector(object):
    '''A vector in 3D space. Coordinate system is cartesian.'''

    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z

    def __str__(self):
        '''Return a printable representation of a vector showing its three coordinates.'''
        return 'Vector(' + ','.join([str(self.x), str(self.y), str(self.z)]) + ')'

    def magnitude(self):
        '''Return the magnitude (length) of this vector.'''
        return sqrt(self.x ** 2 + self.y ** 2 + self.z ** 2)

    def __mul__(self, scalar):
        '''Multiply this vector by a scalar, returning a new vector.'''
        return Vector(self.x * scalar, self.y * scalar, self.z * scalar)

    def __truediv__(self, scalar):
        '''Divide this vector by a scalar returning a new vector.'''
        return Vector(self.x / scalar, self.y / scalar, self.z / scalar)

    def normalise(self):
        '''Return a normalised version of this vector, such that its magnitude is 1.'''
        magnitude = self.magnitude()
        if magnitude == 0:
            # Somehow we have a degenerate vector.
            return self
        else:
            return self / self.magnitude()

    def angle(self, other):
        '''Return the angle in radians between this vector and another vector.'''
        dp = self.dot_product(other)
        return acos(dp / self.magnitude() * other.magnitude())

    def dot_product(self, other):
        '''Return the dot product of this vector and another vector.'''
        return self.x * other.x + self.y * other.y + self.z * other.z

    def add_vector(self, other):
        '''Add this vector to another vector, returning a new vector as the result.'''
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def sub_vector(self, other):
        '''Subtract another vector from this vector, returning a new vector as the result.'''
        return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

    def negate_vector(self):
        '''Negate all the components of this vector, returning a new vector as the result.'''
        return Vector(-self.x, -self.y, -self.z)

    def reflect_about_vector(self, other):
        '''Reflect this vector about another vector, returning a new vector as the result.'''
        normal = other.normalise()
        dp = self.dot_product(other) 
        return self.sub_vector(normal * (2 * dp))

class Plane(object):
    '''A plane in 3D space, represented by a point and a normal vector.'''
    def __init__(self, surface, point, normal_vector):
        self.surface = surface
        self.point = point
        self.normal_vector = normal_vector

    def __str__(self):
        '''Return a printable representation of this plane showing its normal and point.'''
        return "Plane(" + ','.join([str(self.normal), str(self.point)]) + ")"

    def intersect_line(self, line):
        '''Return a list of the intersections between a line and this plane.
        The line is represented as a segment, but is assumed to extend infinitely
        forwards and backwards from its start and end. A line can intersect a plane 
        zero, one or infinitely many times. The infinite case occurs when the
        line is within the plane. In that case we return zero intersections.
        '''
        line_vector = line.point2.sub_vector(line.point1)
        normal = self.normal_vector
        numerator = self.point.sub_vector(line.point1).dot_product(normal)
        denom = line_vector.dot_product(normal)
        if near_zero(denom):
            if near_zero(numerator):
                # line is inside the plane, infinite intersections
                return []
            else:
                # line is parallel to the plane, zero intersections
                return []
        else:
            d = numerator / denom
            if d > intersect_epsilon:
                return [line.point1.add_vector(line_vector * d)]
            else:
                return []

    def normal(self, _point):
        '''Return a normal vector to the plane.'''
        return self.normal_vector

class Line(object):
    '''A line segment in 3D space, represented by its start and end points.'''

    # The start and end points are represented as Vectors
    def __init__(self, point1, point2):
        self.point1 = point1
        self.point2 = point2

    def __str__(self):
        '''Return a printable representation of a line showing its start and end points.'''
        return "Line(" + ','.join([str(self.point1), str(self.point2)]) + ')'

class Sphere(object):
    '''A sphere in 3D space, represented by its center point and radius.'''

    # The center point is represented as a vector.
    def __init__(self, surface, center=Vector(0,0,0), radius=1):
        self.surface = surface
        self.center = center
        self.radius = radius

    def intersect_line(self, line):
        '''Return a list of the intersections between a line and this sphere.
        The line is represented as a segment, but is assumed to extend infinitely
        forwards and backwards from its start and end. A line can intersect a sphere
        zero, one or two times only.
        '''
        center = self.center
        radius = self.radius
        point1 = line.point1
        point2 = line.point2
        x1, y1, z1 = point1.x, point1.y, point1.z
        x2, y2, z2 = point2.x, point2.y, point2.z
        x3, y3, z3 = center.x, center.y, center.z

        delta_x = x2 - x1
        delta_y = y2 - y1
        detla_z = z2 - z1

        a = (x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2
        b = 2 * (((x2 - x1) * (x1 - x3)) +
                 ((y2 - y1) * (y1 - y3)) +
                 ((z2 - z1) * (z1 - z3)))
        c = (x3 ** 2 + y3 ** 2 + z3 ** 2 +
             x1 ** 2 + y1 ** 2 + z1 ** 2 - 
            (2 * ((x3 * x1) + (y3 * y1) + (z3 * z1))) -
            (radius ** 2))
        behaviour = (b * b) - (4 * a * c)

        if behaviour < 0:
            # The line does not interesect the sphere.
            return []
        else:
            result = []
            intersect1 = ((-b) + sqrt(behaviour)) / (2*a)
            # ensure the intersect is in the positive direction along the line
            if intersect1 > intersect_epsilon:
                coord1 = Vector(
                           x1 + ((x2 - x1) * intersect1)
                         , y1 + ((y2 - y1) * intersect1)
                         , z1 + ((z2 - z1) * intersect1))
                result.append(coord1) 
            intersect2 = ((-b) - sqrt(behaviour)) / (2*a)
            if intersect2 > intersect_epsilon:
                coord2 = Vector(
                           x1 + ((x2 - x1) * intersect2)
                         , y1 + ((y2 - y1) * intersect2)
                         , z1 + ((z2 - z1) * intersect2))
                result.append(coord2) 
            return result

    def normal(self, point):
        '''Return a vector perpendicular to the surface of this sphere
        at a point on its surface.
        '''
        return point.sub_vector(self.center)

class Scene(object):
    '''A scene of objects and lights.'''

    def __init__(self, objects, lights):
        self.objects = objects
        self.lights = lights

    def __str__(self):
        return "Scene(" + ','.join([str(self.objects), str(self.lights)]) + ")"

class Surface(object):
    '''The surface properties of an object consisting of:

          Its reponse to different lighting conditions:
          - the reflective coefficient [0,1]
          - the ambient coefficient [0,1]
          - the diffuse coefficient [0,1]
          - the specular coefficient [0,1]
          - the specular exponent

          Its inherent colour:
          - the red coefficient [0,1]
          - the green coefficient [0,1]
          - the blue coefficient [0,1]
    '''
    def __init__(self, reflect_coeff, ambient_coeff,
                 diffuse_coeff, specular_coeff, specular_exponent,
                 red_coeff, green_coeff, blue_coeff):
        self.reflect_coeff = reflect_coeff
        self.ambient_coeff = ambient_coeff
        self.diffuse_coeff = diffuse_coeff
        self.specular_coeff = specular_coeff
        self.specular_exponent = specular_exponent
        self.red_coeff = red_coeff
        self.green_coeff = green_coeff
        self.blue_coeff = blue_coeff

    def __str__(self):
        '''Return a printable representation of a line showing its components.'''
        return ('Surface(' + ','.join([str(x) for x in
                [self.reflect_coeff, self.ambient_coeff,
                self.diffuse_coeff, self.specular_coeff,
                self.specular_exponent,
                self.red_coeff, self.green_coeff,
                self.blue_coeff]]) + ')')

class Light(object):
    '''A white point light source represented by its point in 3D space and its
     intensity [0,1]. This light shines equally in all directions.
    '''
    def __init__(self, point, intensity):
        self.point = point
        self.intensity = intensity

def render_pixels(scene, args):
    '''Render each pixel in the image plane given the scene, image width,
    image height, and focal length. Pixels are generated left-to-right,
    top-to-bottom, starting at (xlo, yhi) going through to (xhi, ylo).

    The image lies in the plane Z=0. The scene is in the positive Z direction,
    and the focal point is at 0,0,-focal_length. The center of the image lies at
    the origin (0,0,0). The Y axis is vertical and the X axis is horizontal.

    The result is an list of list of pixels, where each pixel is represented
    as an (R,G,B) tuple, where each R,G,B intensity is in the range [0,255].
    '''
    focal_point = Vector(0, 0, -args.focal)
    width, height = args.width, args.height
    rows = []
    # compute the X,Y bounds of the image plane
    x_low = -width / 2
    y_low = -height / 2
    x_high = x_low + width - 1
    y_high = y_low + height - 1
    # Render each pixel starting at the top-left corner of the image,
    # scanning left to right, top to bottom, ending a the bottom-right
    # corner.
    for y in range(int(y_high), int(y_low), -1):
        this_row = []
        for x in range(int(x_low), int(x_high)):
            # the position of this pixel in the image plane (Z=0)
            pixel_point = Vector(x, y, 0)
            # a line segment from the focal point to the pixel
            ray = Line(focal_point, pixel_point)
            # trace the ray along this line segment
            pixel = ray_trace(ray, scene, recurse_cutoff=args.recurse)
            this_row.append(pixel)
        rows.append(this_row)
    return rows

def ray_trace(ray, scene, recurse_cutoff=0):
    '''Cast a ray through the focal point and a pixel and
    calculate the colour of the pixel based on any object
    intersections. If there are no intersections then set
    the colour to black. If recurse_cutoff > 0,
    then recursively compute the colour of the reflected ray.
    The recurse_cutoff determines how many times to reflect
    a ray in the scene, and prevents infinite recursion.
    '''
    intersection = get_intersect(ray, scene.objects)
    if intersection == None:
        # The ray did not hit any objects, return a black pixel.
        return (0, 0, 0)
    else:
        # The ray hit at least one object. Compute the colour
        # of the intersection closest to the origin of the ray
        # in the direction of the ray (not behind the origin).
        hit_point, hit_obj = intersection
        reflect_coeff = hit_obj.surface.reflect_coeff
        reflected_colour = (0,0,0)
        # Only compute the colour of the reflected ray if
        # this object is reflective and the recurse_cutoff is
        # above zero.
        if reflect_coeff > 0 and recurse_cutoff > 0:
            # The incoming vector to the intersection point
            in_vector = hit_point.sub_vector(ray.point1)
            # The normal to the object at the intersection point
            normal = hit_obj.normal(hit_point)
            # The reflected vector from the intersection point
            out_vector = in_vector.reflect_about_vector(normal).normalise()
            # A point on the reflected ray.
            out_point = hit_point.add_vector(out_vector)
            # The reflected ray from the intersection point.
            reflect_ray = Line(hit_point, out_point)
            # Recursively compute the colour of the reflected ray.
            reflected_colour = ray_trace(reflect_ray, scene, recurse_cutoff-1)
        # Compute the diffuse and specular reflection properties of the
        # intersection point given the direct illumination from ambient light
        # and the lights in the scene (ie not the reflected component).
        diffuse, specular = illuminate_point_for_each_light(scene, hit_point, ray.point1, hit_obj)
        # Combine all the illumination properties of the intersection point
        # to form the final pixel colour.
        return to_pixel(diffuse, specular, reflected_colour, hit_obj.surface)

def illuminate_point_for_each_light(scene, obj_point, viewer_point, obj):
    '''Compute the total intensity of a pixel by summing the contributions
    of each light source and the ambient light.

    Diffuse intenstities and specular intensities are summed
    independently because the first depends on the colour of
    the object and the second depends only on the colour of
    the light source (which is always white at the moment).
    '''
    diffuse, specular = 0, 0 
    # Compute the illumation for each light source.
    for light in scene.lights:
        shading = shade(scene.objects, obj_point, viewer_point, obj, light)
        diffuse += shading[0]
        specular += shading[1] 
    return (diffuse, specular)

def shade(objects, obj_point, viewer_point, obj, light):
    '''Shade a point for one light source, given the objects in the scene,
    the point on the surface of the object, the focal point of the camera,
    the object itself and the intensity of the light source. Shadows are
    supported by checking if the line between the object point and the
    light source is intersected by any objects.

    The result is a pair containing:
       1. the diffuse + ambient illumination
       2. the specular illumination

    Diffuse and ambient illumination are added together because they
    both depend on the colour of the object surface, whereas specular
    illumination only depends on the colour of the light source.
    '''
    surface = obj.surface
    # Vector from object point to light.
    light_vector = light.point.sub_vector(obj_point).normalise() 
    # Line segment from object point to light.
    line = Line(obj_point, obj_point.add_vector(light_vector))
    # Check if any object intersects the line from object point to light,
    # to test if the object point is in shadow or if it is illuminated by
    # the light.
    intersection = get_intersect(line, objects)
    if intersection == None:
        # The object point is illuminated by the light (not in shadow).
        # Compute the diffuse and specular reflections.
        # Normal to the surface of the object at the intersection point.
        normal = obj.normal(obj_point).normalise() 
        # Vector from object point to "viewer" (origin of the incoming ray).
        viewer_vector = viewer_point.sub_vector(obj_point).normalise() 
        # Compute the diffuse illumination
        diff = diffuse(surface, light_vector, normal, light)
        # Compute the specular illumination
        spec = specular(surface, light_vector, viewer_vector, normal, light)
        return (surface.ambient_coeff + diff, spec)
    else:
        # The object is in a shadow, return just the ambient illumination.
        return (surface.ambient_coeff, 0)

def get_intersect(line, objs):
    '''Compute the intersection of the line with all the objects in the scene.
    Return the object which is closest to the positive side of the line segment
    and the point of intersection. If no intersections are found then return
    None.
    '''
    result_point = None
    for obj in objs:
        intersections = obj.intersect_line(line)
        for point in intersections:
            if result_point == None:
                result_point = point
                result_obj = obj
                result_distance = point.sub_vector(line.point1).magnitude()
            else:
                this_distance = point.sub_vector(line.point1).magnitude()
                if this_distance < result_distance:
                    result_point = point
                    result_obj = obj
                    result_distance = this_distance
    if result_point == None:
        return None
    else:
        return (result_point, result_obj)

def diffuse(surface, light_vector, normal, light):
    '''Return the diffuse illumination of a point for a given
    surface, light, and normal to the surface.
    '''
    if near_zero(surface.diffuse_coeff):
        # This surface does not respond to diffuse illumination.
        return 0
    else:
        product = normal.dot_product(light_vector)
        if product < 0:
            # The surface is not facing the light.
            return 0
        else:
            # The surface is facing the light.
            return light.intensity * surface.diffuse_coeff * product

def specular(surface, light_vector, viewer_vector, normal, light):
    '''Return the specular illumination of a point for a given surface,
    light, viewer and normal to the surface. We use the Phong model
    for specular reflection.
    '''
    if near_zero(surface.specular_coeff):
        # This surface does not respond to specular illumination.
        return 0
    else:
        sum_light_view = light_vector.add_vector(viewer_vector)
        h_vector = sum_light_view / sum_light_view.magnitude()
        product = normal.dot_product(h_vector)
        if product < 0:
            return 0
        else:
            return (surface.specular_coeff * light.intensity *
                   (product ** surface.specular_exponent))

def to_pixel (diffuse, specular, reflected, surface):
    '''Convert diffuse, specular and reflected intensity values into a pixel
    colour, based on the properties of the surface. The reflected light
    is coloured, but the direct light source is assumed to be white (grey).
    '''
    reflect_red, reflect_green, reflect_blue = reflected
    reflect_coeff = surface.reflect_coeff
    reflect_red *= reflect_coeff
    reflect_green *= reflect_coeff
    reflect_blue *= reflect_coeff
    def mk_int(coeff):
        return ((coeff * diffuse) + specular) * 255
    red = clip(mk_int(surface.red_coeff) + reflect_red)
    green = clip(mk_int(surface.green_coeff) + reflect_green)
    blue = clip(mk_int(surface.blue_coeff) + reflect_blue)
    return red, green, blue

def clip(intensity):
    '''Ensure the final pixel intensities are in the range 0-255.'''
    intensity = int(round(intensity))
    return min(max(0, intensity), 255)

def near_zero(x):
    '''Test if a number is close to zero.'''
    return abs(x) <= 0.0005

# XXX This function does not do sufficent error checking of the input.
# We really need to write a proper scene parser and use a more
# friendly scene description language.
def read_scene(scene_file):
    '''Read a scene description file, returning a Scene object containing
    a list of lights and a list of objects in the scene.'''
    objects = []
    lights = []
    # Each object receives the most recently seen surface description,
    # which is initialised to the default properties.
    surface = default_surface_properties
    for line in scene_file:
        fields = line.split()
        tag = fields[0]
        if tag == 'surface' and len(fields) == 9:
            refl_coeff = float(fields[1])
            amb_coeff = float(fields[2])
            diff_coeff = float(fields[3])
            spec_coeff = float(fields[4])
            spec_exp = float(fields[5])
            r_coeff = float(fields[6])
            g_coeff = float(fields[7])
            b_coeff = float(fields[8])
            surface = Surface(refl_coeff,
                      amb_coeff,
                      diff_coeff,
                      spec_coeff,
                      spec_exp,
                      r_coeff,
                      g_coeff,
                      b_coeff)
        elif tag == 'light' and len(fields) == 5:
            intensity, x, y, z = fields[1:] 
            point = Vector(float(x), float(y), float(z))
            light = Light(point, float(intensity))
            lights.append(light)
        elif tag == 'sphere' and len(fields) == 5:
            radius, x, y, z = fields[1:]
            center = Vector(float(x), float(y), float(z))
            sphere = Sphere(surface, center, float(radius))
            objects.append(sphere)
        elif tag == 'plane' and len(fields) == 7:
            px, py, pz, vx, vy, vz = fields[1:]
            point = Vector(float(px), float(py), float(pz))
            normal = Vector(float(vx), float(vy), float(vz))
            plane = Plane(surface, point, normal)
            objects.append(plane)
        else:
            # Skip over any lines which aren't in a recognised
            # format.
            print('Unrecognised scene line: ' + line)
    return Scene(objects, lights)

# Default surface properties for an object.
default_surface_properties = Surface(0, 0.1, 0.1, 0.1, 10, 0.1, 0.1, 0.1)

def cmdline_args():
    '''Parse the command line arguments of the program.'''
    description = 'Ray - a simple Ray Tracer'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--width',
                    metavar='WIDTH',
                    type=int,
                    default=500,
                    help='image width (default 500)') 
    parser.add_argument('--height',
                    metavar='HEIGHT',
                    type=int,
                    default=500,
                    help='image height (default 500)') 
    parser.add_argument('--focal',
                    metavar='FOCAL',
                    type=int,
                    default=1000,
                    help='focal length (default 1000)') 
    parser.add_argument('--out',
                    metavar='OUT',
                    type=str,
                    default='out.png',
                    help='output image file (default out.png)') 
    parser.add_argument('--recurse',
                    type=int,
                    default=0,
                    help='how many recursive levels of reflection to consider (default 0)')
    parser.add_argument('--noshadows',
                    action='store_true',
                    help='Do not render shadows')
    parser.add_argument('scene',
                    metavar='SCENE',
                    type=argparse.FileType('r'),
                    help='input scene file')
    return parser.parse_args()

# Starting point for the program.
def main():
    # Parse the command line arguments.
    args = cmdline_args()
    # Read the input scene file.
    scene = read_scene(args.scene)
    # Render the scene.
    image = render_pixels(scene, args)
    # Save the resulting image to the output file.
    write_image(image, args.out) 

if __name__ == '__main__':
    main()
