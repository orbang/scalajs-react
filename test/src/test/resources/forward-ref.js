var FancyButton = React.forwardRef(function (props, ref) {
  return React.createElement(
    "button",
    { ref: ref, className: "FancyButton" },
    props.children
  );
});