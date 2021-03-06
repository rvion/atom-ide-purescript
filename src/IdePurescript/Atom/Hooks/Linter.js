// module IdePurescript.Atom.Hooks.Linter

exports.register = function(registry) {
  return function(options) {
    return function() {
      return registry.register(options);
    };
  };
};

exports.deleteMessages = function (linter) {
  return function () {
    linter.deleteMessages();
    return {};
  };
};

exports.setMessages = function (linter) {
  return function (messages) {
    return function () {
      linter.setMessages(messages);
      return {};
    };
  };
};

exports.getMessages = function (linter) {
  return function () {
    return Array.from(linter.getMessages());
  };
};

exports.getEditorLinter = function (linterMain) {
  return function (editor) {
    return function () {
      return linterMain.getEditorLinter(editor);
    };
  };
};

exports.getMarkerBufferRange = function (editorLinter) {
  return function (message) {
    return function() {
      var marker = editorLinter.markers.get(message);
      if (marker && marker.isValid()) {
        // Newer linter versions have different marker type here, Marker instead of TextEditorMarker or whatever
        var update = marker.getBufferRange || marker.getRange;
        return update && update.call(marker);
      } else {
        return null;
      }
    };
  };
};
