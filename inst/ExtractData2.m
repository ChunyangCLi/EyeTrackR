
function LOC = ExtractData2(framedir, poster, coordinate, outputcsv, outputimg, ibegin, iend)

% Input parameters
% framedir: the directory of the video frames
% poster: the source of the clear version of the poster image
% coordinate: the x and y coordinates of the crosshair in terms of the
% scene (Only two columns and no headers)
% outputcsv: the directory and name of the output csv file
% outputimg: the directory of the output image file, put NaN if you don't
% want the output matching images saved
% ibegin: the starting frame
% iend: the ending frame

% Output
% The coordinates of the crosshair in terms of the poster

% call function example
% framedir = 'Frames\';
% poster = 'poster.jpg';
% coordinate = coordinate;
% outputcsv = 'locations_testing.csv';
% outputimg = 'Poster_Output/';
% ibegin = 1;
% iend = 5;
% LOC = ExtractData2(framedir, poster, coordinate, outputcsv, outputimg, ibegin, iend)

framesdir2 = strcat(framedir, '*.jpg');
frames = dir(framesdir2);
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
% read a standard poster
clear = imread(poster);
k = 1;
filename_temp = [framedir, frames(k).name];
size_std = size(rgb2gray(imread(filename_temp)));
clear = imresize(clear, size_std);
%% Find Image Rotation and Scale Using Automated Feature Matching
% This example shows how to automatically align two images that differ by a
% rotation and a scale change. It closely parallels another example titled
% <matlab:showdemo ('RotationFitgeotransExample') Find Image Rotation and Scale>.
% Instead of using a manual approach to register the two images, it
% utilizes feature-based techniques found in the Computer Vision System
% Toolbox(TM) to automate the registration process.

% LOC = NaN indicates that the template matching didn't find the red cross chair
% LOC = (0 0) indicates that the matching fails
LOC = zeros([length(frames), 2]);
AA = coordinate;

% if ~exist('ibegin', 'iend')
%     ibegin = 1;
%     iend = length(frames);
% end
if ibegin > iend
tempvar = iend;
iend = ibegin;
ibegin = tempvar;
end


for j = ibegin:iend%:0.25*length(frames)
%% Step 1: Read Image
% Bring an image into the workspace.
same_resolution = clear;
original = rgb2gray(same_resolution);


%% Step 2: Resize and Rotate the Image
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

%%
% Compute the inverse transformation matrix.
Tinv  = tform.invert.T;
ss = Tinv(2,1);
sc = Tinv(1,1);
scale_recovered = sqrt(ss*ss + sc*sc);
theta_recovered = atan2(ss,sc)*180/pi;

%%
% The recovered values should match your scale and angle values selected in
% *Step 2: Resize and Rotate the Image*.

%% Step 6: Recover the Original Image
% Recover the original image by transforming the distorted image.
outputView = imref2d(size(original));
% recovered  = imwarp(distorted,tform,'OutputView',outputView);
recovered2a = imwarp(distorted2(:, :, 1),tform,'OutputView',outputView);
recovered2b = imwarp(distorted2(:, :, 2),tform,'OutputView',outputView);
recovered2c = imwarp(distorted2(:, :, 3),tform,'OutputView',outputView);

if AA(j, 1) < 0 || AA(j, 2) < 0
    LOC(j, :) = [NA NA];
else
picture = zeros(480, 640);
picture((round(AA(j, 2)) - 4):(round(AA(j, 2)) + 4), (round(AA(j, 1)) - 4):(round(AA(j, 1)) + 4)) = ones(9, 9);
% picture = uint8(picture);
outputView = imref2d(size(picture));
picr = imwarp(picture, tform, 'OutputView', outputView);
[x1, y1] = find(picr == 1);
LOC(j, :) = [mean(y1) mean(x1)];
end

colored = cat(3, recovered2a, recovered2b, recovered2c);
%% Output the extracted poster images

   if (isnan(outputimg) ~= 0)
   outputimg1 = strcat(outputimg, 'matching%d');
   filename3 = sprintf(outputimg1, j);
   Compare = [clear colored];
   figure('visible', 'off'); imshow(Compare); hold on; plot(LOC(j, 1), LOC(j, 2),'r*');
   saveas(gcf, filename3, 'jpeg')
   end

catch

   if (isnan(outputimg) ~= 0)
   outputimg2 = strcat(outputimg, 'matching%d.jpg');
   filename3 = sprintf(outputimg2, j);
   imwrite(distorted2, filename3);
   LOC(j, :) = [NaN NaN];
   end

end

fprintf('Just finished iteration #%d\n', j);
close all;


end

csvwrite(outputcsv, LOC)


end


