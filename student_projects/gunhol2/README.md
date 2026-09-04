# Machine learning for viability classification of Trypan blue–stained tomato pollen
## Background
<p>
Pollen viability is an important trait in tomato heat stress research. TTC and Alexander
staining are commonly used to evaluate pollen viability, but these methods can require
additional reagents and more complex staining procedures. Trypan blue is relatively
inexpensive and easy to prepare, which makes it a useful option for laboratories with
limited resources. However, manual counting is labor-intensive, and there is currently no
established model designed for classifying Trypan blue-stained tomato pollen. There are
several challenges in developing this type of model. First, the images contain a noisy
background and a large amount of debris. Because the pollen is collected directly into
tubes without a mesh filter, anther tissue and other particles can remain in the sample.
Second, dead pollen does not have one shape. Dead pollen can show different
morphological shapes depending on the level of damage. This makes it more difficult to
classify dead pollen. Third, counting needs to follow the rules of a hemocytometer,
which are not considered in most existing models. Under a Poisson counting
assumption, counting more pollen grains reduces sampling error.
</p>
## Plan
<p>
  <list>
1. Tomato will be grown in a greenhouse under control and high temperature. High
temperature will be used to increase the non-viable pollen. Open flowers will then be
collected from each treatment.

2. Each pollen will be collected from anther and mixed with pollen viability solution and
1% Trypan blue and stained for 3 minutes. The stained pollen will be loaded onto a
hemocytometer, and images will be collected using a light microscope.

3. For model development, pollen grains in the images will be manually annotated as
live or dead, and an open source object detection model will be retrained on these
images to detect and classify each grain.

4. The model will be validated for a
  </list>
</p>
