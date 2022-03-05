"use strict";

exports._getFocusedBlockMetadata = just => nothing => () => {
  const focusedBlockMetadata = window.roamAlphaAPI.ui.getFocusedBlock();
  if (focusedBlockMetadata) {
    return just(focusedBlockMetadata);
  } else {
    return nothing;
  }
};

exports._findBlock = uid => () =>
  window.roamAlphaAPI
    .data
    .q(`[:find (pull ?e [*])
         :in $ ?uid
         :where [?e :block/uid ?uid]
        ]`, uid);

exports.setBlockString = string => uid => () => {
  window.roamAlphaAPI
    .data
    .block
    .update({block: {
      uid: uid,
      string: string,
    }});
};
