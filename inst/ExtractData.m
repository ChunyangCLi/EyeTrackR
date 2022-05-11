
function  LOC = ExtractData(framedir, poster, outputcsv, outputimg, ibegin, iend)

% Input parameters
% framedir: the directory of the video frames
% poster: the source of the clear version of the poster image
% outputcsv: the directory and name of the output csv file
% outputimg: the directory of the output image file, put NaN if you don't
% want the output matching images saved
% ibegin: the starting frame
% iend: the ending frame

% Output
% The coordinates of the crosshair in terms of the poster and a folder of
% the matched poster images.

% call function example
% framedir = 'Frames\';
% poster = 'poster.jpg';
% outputcsv = 'locations_testing.csv';
% outputimg = 'Poster_Output/';
% ibegin = 1;
% iend = 5;
% LOC = ExtractData(framedir, poster, outputcsv, outputimg, ibegin, iend)


framesdir2 = strcat(framedir, '*.jpg');
frames = dir(framesdir2);
%% Sort the images
A = frames;
Afields = fieldnames(A);
Acell = struct2cell(A);
sz = size(Acell);
% Convert to a matrix
Acell = reshape(Acell, sz(1), []);      % Px(MxN)
% Make each field a column
Acell = Acell';                         % (MxN)xP
% Sort by first field "name"
[~, idx] = sort_nat(Acell(:,1));
Acell = Acell(idx, :);
% Put back into original cell array format
Acell = reshape(Acell', sz);
% Convert to Struct
Asorted = cell2struct(Acell, Afields, 1);
frames = Asorted;
%%

% read and resize a standard poster
 clear = imread(poster);
% k = 1;
% filename_temp = [framedir, frames(k).name];
% size_std = size(rgb2gray(imread(filename_temp)));
% clear = imresize(clear, size_std);

%% Find Image Rotation and Scale Using Automated Feature Matching

% LOC = NaN indicates that the matching fails
LOC = zeros([length(frames), 2]);

if ibegin > iend
tempvar = iend;
iend = ibegin;
ibegin = tempvar;
end

if isnan(ibegin)
ibegin = 1;
end

if isnan(iend)
iend = length(frames);
end

for j = ibegin:iend%:0.25*length(frames)

%% Step 1: Covert the image to grayscale
original = rgb2gray(clear);

%% Step 2: Read the images from the video frame
filename = [framedir, frames(j).name];
distorted = rgb2gray(imread(filename));
distorted2 = imread(filename);

%% Step 3: Find Matching Features Between Images
% Detect features in both images.
ptsOriginal  = detectSURFFeatures(original, 'MetricThreshold', 200, 'NumOctaves', 4);
ptsDistorted = detectSURFFeatures(distorted, 'MetricThreshold', 200, 'NumOctaves', 4);

%%
% Extract feature descriptors.
[featuresOriginal,   validPtsOriginal]  = extractFeatures(original,  ptsOriginal);
[featuresDistorted, validPtsDistorted]  = extractFeatures(distorted, ptsDistorted);

try
%%
% Match features by using their descriptors.
indexPairs = matchFeatures(featuresOriginal, featuresDistorted);

%%
% Retrieve locations of corresponding points for each image.
matchedOriginal  = validPtsOriginal(indexPairs(:,1));
matchedDistorted = validPtsDistorted(indexPairs(:,2));

%%
% Show point matches. Notice the presence of outliers.
% figure;
% showMatchedFeatures(original,distorted,matchedOriginal,matchedDistorted);
% title('Putatively matched points (including outliers)');

%% Step 4: Estimate Transformation
[tform, inlierDistorted, inlierOriginal] = estimateGeometricTransform(...
    matchedDistorted, matchedOriginal, 'similarity');

%% Step 6: Recover the Original Image
% Recover the original image by transforming the distorted image.
outputView = imref2d(size(original));
recovered2a = imwarp(distorted2(:, :, 1),tform,'OutputView',outputView);
recovered2b = imwarp(distorted2(:, :, 2),tform,'OutputView',outputView);
recovered2c = imwarp(distorted2(:, :, 3),tform,'OutputView',outputView);
colored = cat(3, recovered2a, recovered2b, recovered2c);
%%

%%
% https://arduinopro.wordpress.com/2015/12/18/red-object-detector-in-live-video-using-matlab/
%% Detect whether the red crosschair is in the extracted poster or not
% Get the binary image for the red objects in the poster
rgbFrame = colored;
redFrame = rgbFrame(:, :, 1);
grayFrame = rgb2gray(rgbFrame);
diffFrame = imsubtract(redFrame, grayFrame);
diffFrame = medfilt2(diffFrame, [3 3]);
binFrame = im2bw(diffFrame, 0.15);

%%% process before morphological operations
stats = regionprops(binFrame, 'BoundingBox');
box.num = size(stats);
box.num = box.num(1);

for k = 1:box.num

   ratio = max(stats(k).BoundingBox(3), stats(k).BoundingBox(4))/min(stats(k).BoundingBox(3), stats(k).BoundingBox(4));

   if ratio < 2 || ratio > 7

            binFrame(round(stats(k).BoundingBox(2)):round(stats(k).BoundingBox(2) + stats(k).BoundingBox(4)), round(stats(k).BoundingBox(1)):round(stats(k).BoundingBox(1) + stats(k).BoundingBox(3))) = 0;

   end


end


stats2 = regionprops(binFrame, 'BoundingBox', 'Orientation', 'Centroid', 'MajorAxisLength');

% Further remove noises if the BoundingBox is too big or too small;
% Delete axises that are too short or too long
axislength = cat(1, stats2.MajorAxisLength);
stats2(axislength < 20) = [];
stats2(axislength > 80) = [];

% Assuming that the crosshair legs are the dominating pixels
% while sum(axislength < median_axis*1/2) >= 1 || sum(axislength > median_axis*2) >= 1
% stats2(axislength < median_axis*1/2) = [];
% axislength = cat(1, stats2.MajorAxisLength);
% stats2(axislength > median_axis*2) = [];
% axislength = cat(1, stats2.MajorAxisLength);
% median_axis = median(axislength);
% end

axislength = cat(1, stats2.MajorAxisLength);
centroids = cat(1, stats2.Centroid);
median_axis = median(axislength);

 x = NaN;
while (isempty(x) ~= 1 || isempty(x2) ~= 1)
      Dist = pdist(centroids, 'euclidean');
      Dist = squareform(Dist, 'tomatrix');
      [x, y] = find(Dist > median_axis*4);
      if (isempty(x) ~= 1)
          stats2(mode(x)) = [];
      end

      [x2, y2] = find(Dist < 0.3*median_axis & Dist ~= 0);
      if (isempty(x2) ~= 1)
          stats2(mode(x2)) = [];
      end

      median_axis = median(axislength);
      centroids = cat(1, stats2.Centroid);

end
%%
centroids = cat(1, stats2.Centroid);
orientations = cat(1, stats2.Orientation);

B = abs(orientations);
[n, bin] = histc(B, 0.0001:10:90.0001);
multiple = find(n > 1);
index    = find(ismember(bin, multiple));

if isempty(multiple) == 1
single = find(n == 1);
if sum(diff(single)) >= 7
index_single = find(ismember(bin, single));
crosshair_two = B(index_single);
coor = centroids(index_single, :);
frame.size = size(binFrame);

    if coor(:, 2) >= frame.size(1)*1/2
       if coor(:, 1) <= frame.size(2)*1/2
           LOC(j, :) = [min(coor(:, 1)), max(coor(:, 2))];
       else
           LOC(j, :) = [max(coor(:, 1)), max(coor(:, 2))];
       end
    else
        if coor(:, 1) <= frame.size(2)*1/2
           LOC(j, :) = [min(coor(:, 1)), min(coor(:, 2))];
       else
           LOC(j, :) = [max(coor(:, 1)), min(coor(:, 2))];
        end
    end

end

else

crosshair = B(index);
idxn = (crosshair <= 45);
idxp = (crosshair > 45);

positive_angle = sum(idxp);
negative_angle = sum(idxn);
pdots = index(idxp);
ndots = index(idxn);
%% Remove the parts that are too far away or too close
if positive_angle >= 2
    x = NaN;
while (isempty(x) ~= 1)
% Find the distances between the centroid again
      pD = pdist(centroids(pdots, :), 'euclidean');
      pD = squareform(pD, 'tomatrix');
      [x, y] = find(pD > median_axis*4);
      if (isempty(x) ~= 1)
          pdots(mode(x)) = [];
      end
      [x, y] = find(pD < 0.3*median_axis & pD ~= 0);
      if (isempty(x) ~= 1)
          pdots(mode(x)) = [];
      end

end

end

if negative_angle >= 2
     x = NaN;
while (isempty(x) ~= 1)
% Find the distances between the centroid
nD = pdist(centroids(ndots, :), 'euclidean');
nD = squareform(nD, 'tomatrix');
[x, y] = find(nD > median_axis*4);
if (isempty(x) ~= 1)
ndots(mode(x)) = [];
end
[x, y] = find(nD < 0.3*median_axis & nD ~= 0);
if (isempty(x) ~= 1)
ndots(mode(x)) = [];
end
end

end

%%
if length(pdots) >= 2 && length(ndots) >= 2
    if abs(diff(orientations(pdots))) > abs(diff(orientations(ndots)))
       LOC(j, :) = mean(centroids(ndots, :));
    else
        LOC(j, :) = mean(centroids(pdots, :));
    end
elseif length(pdots) >= 2
    LOC(j, :) = mean(centroids(pdots, :));
elseif length(ndots) >= 2
    LOC(j, :) = mean(centroids(ndots, :));
elseif (length(pdots) == 1) && (length(ndots) == 1)
   try
    slope1 = tand(crosshair(idxp));
    slope2 = tand(crosshair(idxn));

    syms x y
    eqn1 = slope1*x + centroids(index(idxp), 2) - slope1*centroids(index(idxp), 1) - y == 0;
    eqn2 = slope2*x + centroids(index(idxn), 2) - slope2*centroids(index(idxn), 1) - y == 0;
    sol = solve([eqn1, eqn2], [x, y]);
    LOC(j, :) = [sol.x, sol.y];
   catch
       LOC(j, :) = [NaN NaN];
       print('solving equation failure')
   end

end

end
%
%  figure;imshow(binFrame)
% st = stats;%(orientations < 0);
% for k = 1 : length(st)
%   thisBB = st(k).BoundingBox;
%   rectangle('Position', [thisBB(1),thisBB(2),thisBB(3),thisBB(4)],...
%   'EdgeColor','r','LineWidth',2 )
% end


%% Output the extracted poster images
outputimg1 = strcat(outputimg, 'matching%d');
filename3 = sprintf(outputimg1, j);
Compare = [clear colored];
figure('visible', 'off'); imshow(Compare); hold on; plot(LOC(j, 1), LOC(j, 2),'r*');
saveas(gcf, filename3, 'jpeg')

catch

outputimg2 = strcat(outputimg, 'matching%d.jpg');
filename3 = sprintf(outputimg2, j);
imwrite(distorted2, filename3);
LOC(j, :) = [NaN NaN];

end

fprintf('Just finished iteration #%d\n', j);
close all;

end

csvwrite(outputcsv, LOC)

end






