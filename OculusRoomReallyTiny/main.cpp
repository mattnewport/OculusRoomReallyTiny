// Modified (cut down / simplified) version of OculusRoomTiny sample from Oculus 0.8 SDK
// Modified by: Matt Newport (matt@mattnewport.com)
// Original copyright / license info below:

/************************************************************************************
Content     :   First-person view test application for Oculus Rift
Created     :   11th May 2015
Authors     :   Tom Heath
Copyright   :   Copyright 2015 Oculus, Inc. All Rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*************************************************************************************/
/// This is an entry-level sample, showing a minimal VR sample, in a simple environment. Use WASD
/// keys to move around, and cursor keys. Dismiss the health and safety warning by tapping the
/// headset, or pressing any key. It runs with DirectX11.

#include <algorithm>
#include <array>
#include <functional>
#include <initializer_list>
#include <memory>
#include <type_traits>
#include <vector>

#include <comdef.h>
#include <comip.h>
#include <d3d11.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>

#include <OVR_CAPI_D3D.h>

#pragma comment(lib, "d3d11.lib")
#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "dxgi.lib")

using namespace DirectX;
using namespace std;

void Validate(bool x, const char* msg) {
    if (!(x)) {
        MessageBoxA(NULL, (msg), "OculusRoomTiny", MB_ICONERROR | MB_OK);
        exit(-1);
    }
}

template <typename T>
const T* temp_ptr(const T&& x) { return &x; }

// Helper to wrap ovr types like ovrSession in a unique_ptr with custom create /
// destroy
const auto create_unique = [](auto* p, auto destroyFunc) {
    return unique_ptr<decay_t<decltype(*p)>, decltype(destroyFunc)>{p, destroyFunc};
};

// Define _com_ptr_t COM smart pointer typedefs for all the D3D and DXGI interfaces we use
#define COM_SMARTPTR_TYPEDEF(x) _COM_SMARTPTR_TYPEDEF(x, __uuidof(x))
COM_SMARTPTR_TYPEDEF(ID3D11BlendState);
COM_SMARTPTR_TYPEDEF(ID3D11Buffer);
COM_SMARTPTR_TYPEDEF(ID3D11DepthStencilState);
COM_SMARTPTR_TYPEDEF(ID3D11DepthStencilView);
COM_SMARTPTR_TYPEDEF(ID3D11Device);
COM_SMARTPTR_TYPEDEF(ID3D11DeviceContext);
COM_SMARTPTR_TYPEDEF(ID3D11InputLayout);
COM_SMARTPTR_TYPEDEF(ID3D11PixelShader);
COM_SMARTPTR_TYPEDEF(ID3D11RasterizerState);
COM_SMARTPTR_TYPEDEF(ID3D11RenderTargetView);
COM_SMARTPTR_TYPEDEF(ID3D11SamplerState);
COM_SMARTPTR_TYPEDEF(ID3D11ShaderResourceView);
COM_SMARTPTR_TYPEDEF(ID3D11Texture2D);
COM_SMARTPTR_TYPEDEF(ID3D11VertexShader);
COM_SMARTPTR_TYPEDEF(ID3DBlob);
COM_SMARTPTR_TYPEDEF(IDXGIAdapter);
COM_SMARTPTR_TYPEDEF(IDXGIDevice1);
COM_SMARTPTR_TYPEDEF(IDXGIFactory);
COM_SMARTPTR_TYPEDEF(IDXGISwapChain);
#undef COM_SMARTPTR_TYPEDEF

struct Window {
    Window(HINSTANCE hinst, LPCWSTR title) : Running{true} {
        WNDCLASSW wc{};
        wc.lpszClassName = L"App";
        wc.style = CS_OWNDC;
        wc.lpfnWndProc = WindowProc;
        wc.cbWndExtra = sizeof(this);
        RegisterClassW(&wc);

        // adjust the window size and show at InitDevice time
        Hwnd =
            CreateWindowW(wc.lpszClassName, title, WS_OVERLAPPEDWINDOW, 0, 0, 0, 0, 0, 0, hinst, 0);

        SetWindowLongPtr(Hwnd, 0, reinterpret_cast<LONG_PTR>(this));
    }

    auto HandleMessages() const {
        MSG msg{};
        while (PeekMessage(&msg, nullptr, 0U, 0U, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        return Running;
    }

    void Run(ovrResult (*MainLoop)(const Window& window)) const {
        auto tryReinit = false;
        while (HandleMessages()) {
            auto res = MainLoop(*this);
            // Try and reinit if display lost, otherwise this is a hard error
            tryReinit = res == ovrError_DisplayLost ? true : tryReinit;
            if (!tryReinit) {
                ovrErrorInfo errorInfo{};
                ovr_GetLastErrorInfo(&errorInfo);
                Validate(OVR_SUCCESS(res), errorInfo.ErrorString);
                break;
            }
            // Sleep a bit before retrying to reduce CPU load while the HMD is disconnected
            Sleep(10);
        }
    }

    static LRESULT CALLBACK WindowProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) {
        auto p = reinterpret_cast<Window*>(GetWindowLongPtr(hWnd, 0));
        switch (Msg) {
        case WM_KEYDOWN:
            p->Keys[wParam] = true;
            break;
        case WM_KEYUP:
            p->Keys[wParam] = false;
            break;
        case WM_DESTROY:
            p->Running = false;
            break;
        default:
            return DefWindowProcW(hWnd, Msg, wParam, lParam);
        }
        if ((p->Keys['Q'] && p->Keys[VK_CONTROL]) || p->Keys[VK_ESCAPE]) p->Running = false;
        return 0;
    }

    HWND Hwnd = nullptr;
    bool Keys[256] = {};

private:
    bool Running = false;
};

struct DirectX11 {
    DirectX11(HWND window, int vpW, int vpH, const LUID* pLuid);

    void ClearAndSetRenderTarget(ID3D11RenderTargetView* rendertarget,
                                 ID3D11DepthStencilView* depthbuffer) const {
        Context->ClearRenderTargetView(rendertarget, data({0.0f, 0.0f, 0.0f, 0.0f}));
        Context->ClearDepthStencilView(depthbuffer, D3D11_CLEAR_DEPTH | D3D11_CLEAR_STENCIL,
                                       1.0f, 0);
        Context->OMSetRenderTargets(1, &rendertarget, depthbuffer);
    }

    void SetViewport(const ovrRecti& vp) const {
        Context->RSSetViewports(
            1, data({D3D11_VIEWPORT{float(vp.Pos.x), float(vp.Pos.y), float(vp.Size.w),
                                    float(vp.Size.h), 0.0f, 1.0f}}));
    }

    auto createBuffer(const D3D11_BUFFER_DESC& desc) {
        ID3D11BufferPtr res;
        Validate(SUCCEEDED(Device->CreateBuffer(&desc, nullptr, &res)), "CreateBuffer failed");
        return res;
    }

    auto createAndFillBuffer(const D3D11_BUFFER_DESC& desc, const D3D11_SUBRESOURCE_DATA& srd) {
        ID3D11BufferPtr res;
        Validate(SUCCEEDED(Device->CreateBuffer(&desc, &srd, &res)), "CreateBuffer failed");
        return res;
    }

    int WinSizeW = 0, WinSizeH = 0;
    ID3D11DevicePtr Device;
    ID3D11DeviceContextPtr Context;
    IDXGISwapChainPtr SwapChain;
    ID3D11Texture2DPtr BackBuffer;
    ID3D11VertexShaderPtr D3DVert;
    ID3D11PixelShaderPtr D3DPix;
    ID3D11InputLayoutPtr InputLayout;
    ID3D11SamplerStatePtr SamplerState;
    ID3D11BufferPtr ConstantBuffer;
};

enum class TextureFill { WHITE, WALL, FLOOR, CEILING };

auto createTexture(ID3D11Device* device, ID3D11DeviceContext* context, TextureFill texFill) {
    constexpr auto widthHeight = 256;

    // Fill texture with requested pattern
    const auto getColor = [texFill](auto x, auto y) {
        enum : uint32_t {
            GRAY = 0xff848484,
            DARK_GRAY = 0xff969696,
            LIGHT_GRAY = 0xffd9d9d9,
            WHITE = 0xffffffff
        };
        switch (texFill) {
            case (TextureFill::WALL): {
                const bool a = y / 4 % 16 == 0, b = x / 4 % 16 == 0;
                const bool c = x / 4 % 32 != 0, d = y / 64 % 2 == 0;
                return a || b && c ^ d ? GRAY : LIGHT_GRAY;
            }
            case (TextureFill::FLOOR):
                return x / 128 ^ y / 128 ? LIGHT_GRAY : DARK_GRAY;
            case (TextureFill::CEILING):
                return x / 4 == 0 || y / 4 == 0 ? DARK_GRAY : LIGHT_GRAY;
        }
        return WHITE;
    };

    uint32_t pix[widthHeight * widthHeight] = {};
    for (auto y = 0u; y < widthHeight; ++y)
        for (auto x = 0u; x < widthHeight; ++x) pix[y * widthHeight + x] = getColor(x, y);

    auto texDesc =
        CD3D11_TEXTURE2D_DESC(DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, widthHeight, widthHeight, 1, 0,
                              D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET);
    texDesc.MiscFlags = D3D11_RESOURCE_MISC_GENERATE_MIPS;
    ID3D11Texture2DPtr tex;
    device->CreateTexture2D(&texDesc, nullptr, &tex);
    ID3D11ShaderResourceViewPtr texSrv;
    device->CreateShaderResourceView(tex, nullptr, &texSrv);
    context->UpdateSubresource(tex, 0, nullptr, pix, widthHeight * sizeof(pix[0]), 0);
    context->GenerateMips(texSrv);

    return texSrv;
}

struct Vertex {
    XMFLOAT3 pos;
    uint32_t c;
    XMFLOAT2 uv;
};

struct TriangleSet {
    struct Box {
        float x1, y1, z1, x2, y2, z2;
        uint32_t c;
    };

    TriangleSet(initializer_list<Box> boxes) {
        for (const auto& b : boxes) AddBox(b);
    }

    void AddBox(const Box& b) {
        const auto modifyColor = [](uint32_t c, XMFLOAT3 pos) {
            const auto length = [](const auto& v) { return XMVectorGetX(XMVector3Length(v)); };
            const auto v = XMLoadFloat3(&pos);
            const auto dist1 = length(v + XMVectorSet(2, -4, 2, 0)),
                       dist2 = length(v + XMVectorSet(-3, -4, 3, 0)),
                       dist3 = length(v + XMVectorSet(4, -3, -25, 0));
            const auto mod =
                (rand() % 160 + 192 * (.65f + 8 / dist1 + 1 / dist2 + 4 / dist3)) / 255.0f;
            const auto saturate = [](float x) { return x > 255 ? 255u : uint32_t(x); };
            const auto r = saturate(((c >> 16) & 0xff) * mod),
                       g = saturate(((c >> 8) & 0xff) * mod), b = saturate(((c >> 0) & 0xff) * mod);
            return (c & 0xff000000) + (r << 16) + (g << 8) + b;
        };

        for (int face = 0; face < 6; ++face) {
            const auto faceDiv3 = face / 3, faceMod3 = face % 3;
            for (int i = 0; i < 6; ++i) {
                Indices.push_back(
                    static_cast<uint16_t>(Vertices.size() + abs(faceDiv3 ? 3 - i : i - 2)));
            }
            for (int v = 0; v < 4; ++v) {
                const auto cv = [&b](int i) {
                    return XMFLOAT3{i & 1 ? b.x1 : b.x2, i & 2 ? b.y1 : b.y2, i & 4 ? b.z1 : b.z2};
                };
                const auto rotr3 = [](int x, int rot) { return x >> rot | (x << (3 - rot)) & 7; };
                const auto p = cv(rotr3(v | faceDiv3 << 2, faceMod3));
                const XMFLOAT2 uvs[] = {{p.x, p.y}, {p.z, p.x}, {p.z, p.y}};
                Vertices.push_back({p, modifyColor(b.c, p), uvs[faceMod3]});
            }
        }
    }

    vector<Vertex> Vertices;
    vector<uint16_t> Indices;
};

struct Model {
    Model(DirectX11& device, const TriangleSet& t, XMFLOAT3 argPos,
          ID3D11ShaderResourceView* tex)
        : Pos(argPos), Tex(tex), NumIndices{size(t.Indices)} {
        auto byteSize = [](const auto& v) { return UINT(size(v) * sizeof(v[0])); };
        VertexBuffer =
            device.createAndFillBuffer(CD3D11_BUFFER_DESC{byteSize(t.Vertices), D3D11_BIND_VERTEX_BUFFER},
                                D3D11_SUBRESOURCE_DATA{data(t.Vertices)});

        IndexBuffer = device.createAndFillBuffer(
            CD3D11_BUFFER_DESC{byteSize(t.Indices), D3D11_BIND_INDEX_BUFFER},
            D3D11_SUBRESOURCE_DATA{data(t.Indices)});
    }

    void Render(DirectX11& dx, const XMMATRIX& projView) const {
        const auto mat = XMMatrixTranslationFromVector(XMLoadFloat3(&Pos)) * projView;

        auto map = D3D11_MAPPED_SUBRESOURCE{};
        dx.Context->Map(dx.ConstantBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, &map);
        memcpy(map.pData, &mat, sizeof(mat));
        dx.Context->Unmap(dx.ConstantBuffer, 0);

        dx.Context->IASetInputLayout(dx.InputLayout);
        dx.Context->IASetIndexBuffer(IndexBuffer, DXGI_FORMAT_R16_UINT, 0);
        const auto vbs = {VertexBuffer.GetInterfacePtr()};
        dx.Context->IASetVertexBuffers(0, UINT(size(vbs)), data(vbs), data({UINT(sizeof(Vertex))}),
                                       data({0u}));
        dx.Context->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

        dx.Context->VSSetShader(dx.D3DVert, nullptr, 0);
        dx.Context->PSSetShader(dx.D3DPix, nullptr, 0);

        const auto samplerStates = {dx.SamplerState.GetInterfacePtr()};
        dx.Context->PSSetSamplers(0, UINT(size(samplerStates)), data(samplerStates));

        const auto texSrvs = {Tex.GetInterfacePtr()};
        dx.Context->PSSetShaderResources(0, UINT(size(texSrvs)), data(texSrvs));
        dx.Context->DrawIndexed(UINT(NumIndices), 0, 0);
    }

    XMFLOAT3 Pos;

private:
    ID3D11ShaderResourceViewPtr Tex;
    ID3D11BufferPtr VertexBuffer, IndexBuffer;
    size_t NumIndices;
};

struct Scene {
    Scene(DirectX11& dx11) {
        auto add = [this, &dx11](auto&&... xs) {
            Models.emplace_back(make_unique<Model>(dx11, forward<decltype(xs)>(xs)...));
        };

        enum : uint32_t {
            GRAY = 0xff404040,
            DARK_GRAY = 0xff696969,
            SILVER = 0xff808080,
            RED = 0xffff0000,
            YELLOW = 0xff505000,
            BLUE = 0xff202050
        };

        auto device = dx11.Device.GetInterfacePtr();
        auto context = dx11.Context.GetInterfacePtr();
        TriangleSet cube{{0.5f, -0.5f, 0.5f, -0.5f, 0.5f, -0.5f, GRAY}};
        add(cube, XMFLOAT3{0, 0, 0}, createTexture(device, context, TextureFill::CEILING));

        TriangleSet spareCube{{0.1f, -0.1f, 0.1f, -0.1f, +0.1f, -0.1f, RED}};
        add(spareCube, XMFLOAT3{0, -10, 0}, createTexture(device, context, TextureFill::CEILING));

        TriangleSet walls{{10.1f, 0.0f, 20.0f, 10.0f, 4.0f, -20.0f, SILVER},      // Left Wall
                          {10.0f, -0.1f, 20.1f, -10.0f, 4.0f, 20.0f, SILVER},     // Back Wall
                          {-10.0f, -0.1f, 20.0f, -10.1f, 4.0f, -20.0f, SILVER}};  // Right Wall
        add(walls, XMFLOAT3{0, 0, 0}, createTexture(device, context, TextureFill::WALL));

        TriangleSet floors{{10.0f, -0.1f, 20.0f, -10.0f, 0.0f, -20.1f, SILVER},     // Main floor
                           {15.0f, -6.1f, -18.0f, -15.0f, -6.0f, -30.0f, SILVER}};  // Bottom floor
        add(floors, XMFLOAT3{0, 0, 0},
            createTexture(device, context, TextureFill::FLOOR));  // Floors

        TriangleSet ceiling{{10.0f, 4.0f, 20.0f, -10.0f, 4.1f, -20.1f, SILVER}};
        add(ceiling, XMFLOAT3{0, 0, 0},
            createTexture(device, context, TextureFill::CEILING));  // Ceiling

        TriangleSet furniture{
            {-9.5f, 0.75f, -3.0f, -10.1f, 2.5f, -3.1f, GRAY},     // Right side shelf verticals
            {-9.5f, 0.95f, -3.7f, -10.1f, 2.75f, -3.8f, GRAY},    // Right side shelf
            {-9.55f, 1.20f, -2.5f, -10.1f, 1.30f, -3.75f, GRAY},  // Right side shelf horiz
            {-9.55f, 2.00f, -3.05f, -10.1f, 2.10f, -4.2f, GRAY},  // Right side shelf
            {-5.0f, 1.1f, -20.0f, -10.0f, 1.2f, -20.1f, GRAY},    // Right railing
            {10.0f, 1.1f, -20.0f, 5.0f, 1.2f, -20.1f, GRAY},      // Left railing
            {1.8f, 0.8f, -1.0f, 0.0f, 0.7f, 0.0f, YELLOW},        // Table
            {1.8f, 0.0f, 0.0f, 1.7f, 0.7f, -0.1f, YELLOW},        // Table Leg
            {1.8f, 0.7f, -1.0f, 1.7f, 0.0f, -0.9f, YELLOW},       // Table Leg
            {0.0f, 0.0f, -1.0f, 0.1f, 0.7f, -0.9f, YELLOW},       // Table Leg
            {0.0f, 0.7f, 0.0f, 0.1f, 0.0f, -0.1f, YELLOW},        // Table Leg
            {1.4f, 0.5f, 1.1f, 0.8f, 0.55f, 0.5f, BLUE},          // Chair Set
            {1.401f, 0.0f, 1.101f, 1.339f, 1.0f, 1.039f, BLUE},   // Chair Leg 1
            {1.401f, 0.5f, 0.499f, 1.339f, 0.0f, 0.561f, BLUE},   // Chair Leg 2
            {0.799f, 0.0f, 0.499f, 0.861f, 0.5f, 0.561f, BLUE},   // Chair Leg 2
            {0.799f, 1.0f, 1.101f, 0.861f, 0.0f, 1.039f, BLUE},   // Chair Leg 2
            {1.4f, 0.97f, 1.05f, 0.8f, 0.92f, 1.10f, BLUE}};      // Chair Back high bar
        for (float f = 5; f <= 9; f += 1)
            furniture.AddBox({-f, 0.0f, -20.0f, -f - 0.1f, 1.1f, -20.1f, DARK_GRAY});  // Left Bars
        for (float f = 5; f <= 9; f += 1)
            furniture.AddBox({f, 1.1f, -20.0f, f + 0.1f, 0.0f, -20.1f, DARK_GRAY});  // Right Bars
        for (float f = 3.0f; f <= 6.6f; f += 0.4f)
            furniture.AddBox({3, 0.0f, -f, 2.9f, 1.3f, -f - 0.1f, GRAY});  // Posts
        add(furniture, XMFLOAT3{0, 0, 0},
            createTexture(device, context, TextureFill::WHITE));  // Fixtures & furniture
    }

    void Render(DirectX11& directx, const XMMATRIX& projView) const {
        for (const auto& model : Models) model->Render(directx, projView);
    }

    vector<unique_ptr<Model>> Models;
};

struct Camera {
    XMVECTOR Pos, Rot;
    auto GetViewMatrix() const {
        const auto forward = XMVector3Rotate(XMVectorSet(0, 0, -1, 0), Rot),
                   up = XMVector3Rotate(XMVectorSet(0, 1, 0, 0), Rot);
        return XMMatrixLookAtRH(Pos, Pos + forward, up);
    }
};

auto createTextureSwapChain(ID3D11Device* device, ovrSession session, int width, int height) {
    // Create and validate the swap texture set and stash it in unique_ptr
    auto desc = ovrTextureSwapChainDesc{};
    desc.Type = ovrTexture_2D;
    desc.Format = OVR_FORMAT_R8G8B8A8_UNORM_SRGB;
    desc.ArraySize = 1;
    desc.Width = width;
    desc.Height = height;
    desc.MipLevels = 1;
    desc.SampleCount = 1;
    desc.StaticImage = ovrFalse;
    desc.MiscFlags = ovrTextureMisc_DX_Typeless;
    desc.BindFlags = ovrTextureBind_DX_RenderTarget;
    ovrTextureSwapChainData* ts{};
    Validate(OVR_SUCCESS(ovr_CreateTextureSwapChainDX(session, device, &desc, &ts)),
             "Failed to create SwapTextureSet.");
    // unique_ptr deleter lambda to clean up the swap texture set
    return create_unique(
        ts, [session](ovrTextureSwapChainData* ts) { ovr_DestroyTextureSwapChain(session, ts); });
}
using TextureSwapChainPtr = decltype(createTextureSwapChain(nullptr, nullptr, 0, 0));

// ovrSwapTextureSet wrapper class that also maintains the render target views and depth stencil
// view needed for D3D11 rendering.
class OvrEyeRT {
    static auto createDepthStencilView(ID3D11Device* device, int width, int height) {
        ID3D11Texture2DPtr tex;
        Validate(SUCCEEDED(device->CreateTexture2D(
                     temp_ptr(CD3D11_TEXTURE2D_DESC(DXGI_FORMAT_D24_UNORM_S8_UINT, width, height, 1,
                                                    1, D3D11_BIND_DEPTH_STENCIL)),
                     nullptr, &tex)),
                 "CreateTexture2D failed");
        ID3D11DepthStencilViewPtr dsv;
        Validate(SUCCEEDED(device->CreateDepthStencilView(tex, nullptr, &dsv)),
                 "CreateDepthStencilView failed");
        return dsv;
    }

    static auto createTexRtvs(ID3D11Device* device, ovrSession session,
                              ovrTextureSwapChainData* tscd) {
        vector<ID3D11RenderTargetViewPtr> res;
        const auto texCount = [tc = 0, session, tscd]() mutable {
            ovr_GetTextureSwapChainLength(session, tscd, &tc);
            return tc;
        }();
        for (int i = 0; i < texCount; ++i) {
            ID3D11Texture2DPtr tex;
            ovr_GetTextureSwapChainBufferDX(session, tscd, i, tex.GetIID(),
                                            reinterpret_cast<void**>(&tex));
            ID3D11RenderTargetViewPtr rtv;
            device->CreateRenderTargetView(
                tex,
                temp_ptr(CD3D11_RENDER_TARGET_VIEW_DESC{D3D11_RTV_DIMENSION_TEXTURE2D,
                                                        DXGI_FORMAT_R8G8B8A8_UNORM}),
                &rtv);
            res.push_back(rtv);
        }
        return res;
    }

public:
    OvrEyeRT(ID3D11Device* device, ovrSession session, ovrSizei size)
        : TextureChain{createTextureSwapChain(device, session, size.w, size.h)},
          TexRtvs{createTexRtvs(device, session, TextureChain.get())},
          dsv{createDepthStencilView(device, size.w, size.h)},
          viewport{{0, 0}, size} {}

    OvrEyeRT(ID3D11Device* device, ovrSession session, ovrEyeType eye, const ovrHmdDesc& hmdDesc)
        : OvrEyeRT{device, session,
                   ovr_GetFovTextureSize(session, eye, hmdDesc.DefaultEyeFov[eye], 1.0f)} {}

    auto GetRtv(ovrSession session) {
        int index = 0;
        ovr_GetTextureSwapChainCurrentIndex(session, TextureChain.get(), &index);
        return TexRtvs[index];
    }

    void Commit(ovrSession session) { ovr_CommitTextureSwapChain(session, TextureChain.get()); }

    TextureSwapChainPtr TextureChain;
    vector<ID3D11RenderTargetViewPtr> TexRtvs;
    ID3D11DepthStencilViewPtr dsv;
    ovrRecti viewport;
};

DirectX11::DirectX11(HWND window, int vpW, int vpH, const LUID* pLuid)
    : WinSizeW{vpW}, WinSizeH{vpH} {
    auto windowSize = RECT{0, 0, WinSizeW, WinSizeH};
    AdjustWindowRect(&windowSize, WS_OVERLAPPEDWINDOW, false);
    SetWindowPos(window, nullptr, 0, 0, windowSize.right - windowSize.left,
                 windowSize.bottom - windowSize.top, SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW);

    IDXGIFactoryPtr dxgiFactory;
    Validate(
        SUCCEEDED(CreateDXGIFactory1(dxgiFactory.GetIID(), reinterpret_cast<void**>(&dxgiFactory))),
        "CreateDXGIFactory1 failed");

    IDXGIAdapterPtr adapter;
    for (int iAdapter = 0; dxgiFactory->EnumAdapters(iAdapter, &adapter) != DXGI_ERROR_NOT_FOUND;
         ++iAdapter) {
        DXGI_ADAPTER_DESC adapterDesc{};
        adapter->GetDesc(&adapterDesc);
        if (!pLuid|| memcmp(&adapterDesc.AdapterLuid, pLuid, sizeof(LUID)) == 0) break;
    }

    const auto DriverType = adapter ? D3D_DRIVER_TYPE_UNKNOWN : D3D_DRIVER_TYPE_HARDWARE;
    const auto createFlags = [] {
#ifdef _DEBUG
        return D3D11_CREATE_DEVICE_DEBUG;
#else
        return D3D11_CREATE_DEVICE_FLAG(0);
#endif
    }();
    Validate(SUCCEEDED(D3D11CreateDeviceAndSwapChain(
                 adapter, DriverType, nullptr, createFlags, nullptr, 0, D3D11_SDK_VERSION,
                 temp_ptr(DXGI_SWAP_CHAIN_DESC{
                     {UINT(WinSizeW), UINT(WinSizeH), {}, DXGI_FORMAT_R8G8B8A8_UNORM},  // Buffer
                     {1},  // SampleDesc
                     DXGI_USAGE_RENDER_TARGET_OUTPUT,
                     2,  // BufferCount
                     window,
                     TRUE,
                     DXGI_SWAP_EFFECT_SEQUENTIAL}),
                 &SwapChain, &Device, nullptr, &Context)),
             "D3D11CreateDeviceAndSwapChain failed");

    // Create backbuffer
    Validate(SUCCEEDED(SwapChain->GetBuffer(0, BackBuffer.GetIID(),
                                            reinterpret_cast<void**>(&BackBuffer))),
             "IDXGISwapChain::GetBuffer() failed");

    // Buffer for shader constants
    ConstantBuffer = createBuffer(CD3D11_BUFFER_DESC(sizeof(XMMATRIX), D3D11_BIND_CONSTANT_BUFFER,
                                                     D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE));
    auto buffs = {ConstantBuffer.GetInterfacePtr()};
    Context->VSSetConstantBuffers(0, UINT(size(buffs)), data(buffs));

    // Set max frame latency to 1
    IDXGIDevice1Ptr DXGIDevice1;
    Validate(SUCCEEDED(Device.QueryInterface(DXGIDevice1.GetIID(), &DXGIDevice1)),
             "QueryInterface failed");
    DXGIDevice1->SetMaximumFrameLatency(1);

    // Set up render states
    // Create and set rasterizer state
    ID3D11RasterizerStatePtr rss;
    Device->CreateRasterizerState(temp_ptr(CD3D11_RASTERIZER_DESC{D3D11_DEFAULT}), &rss);
    Context->RSSetState(rss);

    // Create and set depth stencil state
    ID3D11DepthStencilStatePtr dss;
    Device->CreateDepthStencilState(temp_ptr(CD3D11_DEPTH_STENCIL_DESC{D3D11_DEFAULT}), &dss);
    Context->OMSetDepthStencilState(dss, 0);

    // Create and set blend state
    ID3D11BlendStatePtr bs;
    Device->CreateBlendState(temp_ptr(CD3D11_BLEND_DESC{D3D11_DEFAULT}), &bs);
    Context->OMSetBlendState(bs, nullptr, 0xffffffff);

    auto compileShader = [](const char* src, const char* target) {
        ID3DBlobPtr blob;
        D3DCompile(src, strlen(src), nullptr, nullptr, nullptr, "main", target, 0, 0, &blob,
                   nullptr);
        return blob;
    };

    // Create vertex shader and input layout
    auto defaultVertexShaderSrc = R"(float4x4 ProjView;
                                         void main(in float4 pos : POSITION,
                                                   in float4 col : COLOR0,
                                                   in float2 tex : TEXCOORD0,
                                                   out float4 oPos : SV_Position,
                                                   out float4 oCol : COLOR0,
                                                   out float2 oTex : TEXCOORD0) {
                                             oPos = mul(ProjView, pos);
                                             oTex = tex;
                                             oCol = col;
                                         })";
    auto vsBlob = compileShader(defaultVertexShaderSrc, "vs_4_0");
    Device->CreateVertexShader(vsBlob->GetBufferPointer(), vsBlob->GetBufferSize(), nullptr,
                               &D3DVert);
    D3D11_INPUT_ELEMENT_DESC defaultVertexDesc[] = {
        {"Position", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, offsetof(Vertex, pos)},
        {"Color", 0, DXGI_FORMAT_B8G8R8A8_UNORM, 0, offsetof(Vertex, c)},
        {"TexCoord", 0, DXGI_FORMAT_R32G32_FLOAT, 0, offsetof(Vertex, uv)},
    };
    Device->CreateInputLayout(defaultVertexDesc, UINT(size(defaultVertexDesc)),
                              vsBlob->GetBufferPointer(), vsBlob->GetBufferSize(), &InputLayout);

    // Create pixel shader
    auto defaultPixelShaderSrc = R"(Texture2D Texture : register(t0);
                                        SamplerState Linear : register(s0);
                                        float4 main(in float4 Position : SV_Position,
                                                    in float4 Color: COLOR0,
                                                    in float2  TexCoord : TEXCOORD0) : SV_Target {
                                            float4 TexCol = Texture.Sample(Linear, TexCoord);
                                            return(Color * TexCol);
                                        })";
    auto psBlob = compileShader(defaultPixelShaderSrc, "ps_4_0");
    Device->CreatePixelShader(psBlob->GetBufferPointer(), psBlob->GetBufferSize(), nullptr,
                              &D3DPix);

    // Create sampler state
    auto ss = CD3D11_SAMPLER_DESC{D3D11_DEFAULT};
    ss.Filter = D3D11_FILTER_ANISOTROPIC;
    ss.AddressU = ss.AddressV = ss.AddressW = D3D11_TEXTURE_ADDRESS_WRAP;
    ss.MaxAnisotropy = 8;
    Device->CreateSamplerState(&ss, &SamplerState);
}

ovrResult MainLoop(const Window& window) {
    auto result = ovrResult{};
    auto luid = ovrGraphicsLuid{};
    // Initialize the session, stash it in a unique_ptr for automatic cleanup.
    const auto session = create_unique(
        [&result, &luid] {
            ovrSession session{};
            result = ovr_Create(&session, &luid);
            return session;
        }(),
        ovr_Destroy);
    if (OVR_FAILURE(result)) return result;

    const auto hmdDesc = ovr_GetHmdDesc(session.get());

    // Setup Device and shared D3D objects (shaders, state objects, etc.)
    // Note: the mirror window can be any size, for this sample we use 1/2 the HMD resolution
    auto directx = DirectX11{window.Hwnd, hmdDesc.Resolution.w / 2, hmdDesc.Resolution.h / 2,
                             reinterpret_cast<LUID*>(&luid)};

    // Create the eye render buffers (caution if actual size < requested due to HW limits).
    OvrEyeRT eyeRTs[] = {{directx.Device, session.get(), ovrEye_Left, hmdDesc},
                         {directx.Device, session.get(), ovrEye_Right, hmdDesc}};

    // Create mirror texture to see on the monitor, stash it in a unique_ptr for automatic cleanup.
    const auto mirrorTexture = create_unique(
        [&result, session = session.get(), &directx] {
            ovrMirrorTextureData* mirrorTexture{};
            result = ovr_CreateMirrorTextureDX(
                session, directx.Device,
                temp_ptr(ovrMirrorTextureDesc{OVR_FORMAT_R8G8B8A8_UNORM_SRGB, directx.WinSizeW,
                                              directx.WinSizeH}),
                &mirrorTexture);
            return mirrorTexture;
        }(),
        // lambda to destroy mirror texture on unique_ptr destruction
        [session = session.get()](ovrMirrorTextureData* mt) {
            ovr_DestroyMirrorTexture(session, mt);
        });
    if (OVR_FAILURE(result)) return result;

    // Initialize the scene and camera
    const auto roomScene = Scene{directx};
    auto mainCam = Camera{XMVectorSet(0.0f, 0.0f, 5.0f, 0), XMQuaternionIdentity()};
    ovr_SetTrackingOriginType(session.get(), ovrTrackingOrigin_FloorLevel);

    // Main loop
    while (window.HandleMessages()) {
        // Handle input
        [&mainCam, &window] {
            const auto forward = XMVector3Rotate(XMVectorSet(0, 0, -0.05f, 0), mainCam.Rot);
            const auto right = XMVector3Rotate(XMVectorSet(0.05f, 0, 0, 0), mainCam.Rot);
            if (window.Keys['W'] || window.Keys[VK_UP]) mainCam.Pos += forward;
            if (window.Keys['S'] || window.Keys[VK_DOWN]) mainCam.Pos -= forward;
            if (window.Keys['D']) mainCam.Pos += right;
            if (window.Keys['A']) mainCam.Pos -= right;
            static auto Yaw = 0.0f;
            if (window.Keys[VK_LEFT])
                mainCam.Rot = XMQuaternionRotationRollPitchYaw(0, Yaw += 0.02f, 0);
            if (window.Keys[VK_RIGHT])
                mainCam.Rot = XMQuaternionRotationRollPitchYaw(0, Yaw -= 0.02f, 0);
        }();

        // Animate the cube
        [&cube = roomScene.Models[0]] {
            static auto cubeClock = 0.0f;
            cube->Pos = {9 * sin(cubeClock), 3, 9 * cos(cubeClock += 0.015f)};
        }();

        // Get both eye poses simultaneously, with IPD offset already included.
        const ovrEyeRenderDesc eyeRenderDesc[] = {
            ovr_GetRenderDesc(session.get(), ovrEye_Left, hmdDesc.DefaultEyeFov[ovrEye_Left]),
            ovr_GetRenderDesc(session.get(), ovrEye_Right, hmdDesc.DefaultEyeFov[ovrEye_Right])};

        auto sensorSampleTime = 0.0;
        const auto eyeRenderPoses = [session = session.get(), &eyeRenderDesc, &sensorSampleTime] {
            array<ovrPosef, 2> res;
            const ovrPosef HmdToEyePoses[] = {eyeRenderDesc[ovrEye_Left].HmdToEyePose,
                                               eyeRenderDesc[ovrEye_Right].HmdToEyePose};
            ovr_GetEyePoses(session, 0, ovrTrue, HmdToEyePoses, data(res), &sensorSampleTime);
            return res;
        }();

        // Render Scene to Eye Buffers
        for (auto eye : {ovrEye_Left, ovrEye_Right}) {
            // Increment to use next texture, just before rendering
            directx.ClearAndSetRenderTarget(eyeRTs[eye].GetRtv(session.get()),
                                            eyeRTs[eye].dsv);
            directx.SetViewport(eyeRTs[eye].viewport);

            // Get the pose information in XM format
            const auto& ori = eyeRenderPoses[eye].Orientation;
            const auto eyeQuat = XMVectorSet(ori.x, ori.y, ori.z, ori.w);
            const auto& pos = eyeRenderPoses[eye].Position;
            const auto eyePos = XMVectorSet(pos.x, pos.y, pos.z, 0);

            // Get view and projection matrices for the eye camera
            const auto CombinedPos = mainCam.Pos + XMVector3Rotate(eyePos, mainCam.Rot);
            const auto finalCam = Camera{CombinedPos, XMQuaternionMultiply(eyeQuat, mainCam.Rot)};
            const auto projT =
                ovrMatrix4f_Projection(eyeRenderDesc[eye].Fov, 0.2f, 1000.0f, ovrProjection_None);
            const auto proj = XMMatrixTranspose(XMMATRIX{&projT.M[0][0]});

            // Render the scene
            roomScene.Render(directx, finalCam.GetViewMatrix() * proj);

            eyeRTs[eye].Commit(session.get());
        }

        // Initialize our single full screen Fov layer.
        const auto ld = [&eyeRTs, &hmdDesc, &eyeRenderPoses, sensorSampleTime] {
            auto res = ovrLayerEyeFov{{ovrLayerType_EyeFov}};
            for (auto eye : {ovrEye_Left, ovrEye_Right}) {
                res.ColorTexture[eye] = eyeRTs[eye].TextureChain.get();
                res.Viewport[eye] = eyeRTs[eye].viewport;
                res.Fov[eye] = hmdDesc.DefaultEyeFov[eye];
                res.RenderPose[eye] = eyeRenderPoses[eye];
                res.SensorSampleTime = sensorSampleTime;
            }
            return res;
        }();
        const auto layers = &ld.Header;
        result = ovr_SubmitFrame(session.get(), 0, nullptr, &layers, 1);
        // exit the rendering loop if submit returns an error, will retry on ovrError_DisplayLost
        if (OVR_FAILURE(result)) return result;

        auto sessionStatus = ovrSessionStatus{};
        ovr_GetSessionStatus(session.get(), &sessionStatus);
        if (sessionStatus.ShouldQuit)
            break;
        if (sessionStatus.ShouldRecenter)
            ovr_RecenterTrackingOrigin(session.get());

        // Display mirror texture on monitor
        ID3D11Texture2DPtr mirrorTexDx;
        ovr_GetMirrorTextureBufferDX(session.get(), mirrorTexture.get(), mirrorTexDx.GetIID(),
                                     reinterpret_cast<void**>(&mirrorTexDx));
        directx.Context->CopyResource(directx.BackBuffer, mirrorTexDx);
        directx.SwapChain->Present(0, 0);
    }

    return result;
}

int WINAPI WinMain(HINSTANCE hinst, HINSTANCE, LPSTR, int) {
    // Initializes LibOVR, and the Rift
    Validate(OVR_SUCCESS(ovr_Initialize(nullptr)), "Failed to initialize libOVR.");

    Window window{hinst, L"Oculus Room Really Tiny (DX11)"};
    window.Run(MainLoop);

    ovr_Shutdown();
}
