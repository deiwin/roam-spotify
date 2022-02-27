const refreshToken = 'redacted';
const clientID = 'redacted';
const clientSecret = 'redacted';

const H_TO_M = 60;
const M_TO_S = 60;
const S_TO_MS = 1000;

const roam = (() => {
  const updateFocusedBlock = f => {
    const focusedBlock = window.roamAlphaAPI.ui.getFocusedBlock();
    if (focusedBlock) {
      const uid = focusedBlock['block-uid'];
      const block = window.roamAlphaAPI.data
        .q(`[:find (pull ?e [*])
             :in $ ?uid
             :where [?e :block/uid ?uid]
            ]`, uid)[0][0];
      window.roamAlphaAPI.data.block.update({block: {
        uid: uid,
        string: f(block.string),
      }});
    }
  };

  return {updateFocusedBlock};
})();

const spotify = (() => {
  const getPlaybackState = async (accessToken) => {
    const response = await fetch('https://api.spotify.com/v1/me/player', {
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${accessToken}`,
      },
    });
    if (response.status === 204) {
      const message = 'Received 204: empty playback state';
      console.warn(message, response);
      throw message;
    } else if (!response.ok) {
      const message = 'Failed to get playback state';
      console.warn(message, response);
      throw message;
    }
    return response.json();
  };

  const pausePlayback = async (accessToken) => {
    const response = await fetch('https://api.spotify.com/v1/me/player/pause', {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${accessToken}`,
      },
    });
    if (!response.ok) {
      const message = 'Failed to pause playback';
      console.warn(message, response);
      throw message;
    }
  };
  const resumePlayback = async (accessToken) => {
    const response = await fetch('https://api.spotify.com/v1/me/player/play', {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${accessToken}`,
      },
    });
    if (!response.ok) {
      const message = 'Failed to resume playback';
      console.warn(message, response);
      throw message;
    }
  };

  const togglePlayback = async (accessToken) => {
    const playbackState = await getPlaybackState(accessToken);
    if (playbackState.is_playing) {
      return pausePlayback(accessToken);
    } else {
      return resumePlayback(accessToken);
    }
  };

  const getAccessToken = async () => {
    const response = await fetch('https://accounts.spotify.com/api/token', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
        'Authorization': `Basic ${btoa(`${clientID}:${clientSecret}`)}`,
      },
      body: new URLSearchParams({
        'grant_type': 'refresh_token',
        'refresh_token': refreshToken,
      }),
    });
    return response.json();
  };

  const withAccessToken = async f => {
    let accessToken = '';
    let timeoutID = -1;

    const updateAccessToken = async () => {
      // Clear the timeout to avoid multiple timeouts being set if this
      // is called again when there already is a timeout in place.
      clearTimeout(timeoutID);

      const tokenResponse = await getAccessToken();
      accessToken = tokenResponse.access_token;

      // Set timeout for five minutes before expiry
      const delay = (tokenResponse.expires_in - (5 * M_TO_S)) * S_TO_MS;
      timeoutID = setTimeout(updateAccessToken, delay);
    };
    await updateAccessToken();

    const get = () => accessToken;
    f(get);
  };

  return {withAccessToken, getPlaybackState, togglePlayback};
})();

const prefixFocusedBlockWithTimestamp = async (accessToken) => {
  const msToHMSTimestamp = ms => {
    const hours = Math.floor(ms / (H_TO_M * M_TO_S * S_TO_MS));
    const minutes = Math.floor(ms / (M_TO_S * S_TO_MS)) % H_TO_M;
    const seconds = Math.floor(ms / S_TO_MS) % M_TO_S;

    const pad = n => String(n).padStart(2, "0");
    return `${pad(hours)}:${pad(minutes)}:${pad(seconds)}`;
  };

  const playbackState = await spotify.getPlaybackState(accessToken);
  const timestamp = msToHMSTimestamp(playbackState.progress_ms);
  roam.updateFocusedBlock(s => `${timestamp} ${s}`);
};


if (false) {
  let getAccessToken = null;
  await spotify.withAccessToken(f => {getAccessToken = f});
  prefixFocusedBlockWithTimestamp(getAccessToken());
}
