<?php include 'head.php'; ?>

<?php 

if (isset($_FILES["image"])) {
  if ($_FILES['image']['error'] > 0)
    $error = "Error while transfering the file";
  else {
    if (!move_uploaded_file($_FILES['image']['tmp_name'], 'data/input_image.jpg'))
      $error = "Error while moving the file.";
    else {
      sleep(0.3);
    }
  }
}
else {
    $error = 'No image sent. <a href="step1.php">Come back</a>';
}

?>

<div id="step-number"><div id="img" class="step2"></div></div>

<?php if (!isset($error)) { ?>

<div id="processed"></div>

<?php } else { 

echo '<div class="errorinfo">' . $error . '</div>' ;

} ?>

<script src="js/processing.js"></script>

<?php include 'foot.php'; ?>
