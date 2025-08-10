import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { composeStories } from '@storybook/react';
import * as stories from './breadcrumb_nav.stories';

const { Default: DefaultStory, WithLongPaths: WithLongPathsStory } = composeStories(stories);

describe('Breadcrumb Navigation', () => {
  it('should render correctly at 480px viewport', async () => {
    const canvas = within(await DefaultStory.render());
    await userEvent.viewport({ width: 480, height: 800 });
    expect(canvas.getByRole('navigation')).toBeInTheDocument();
  });

  it('should render correctly at 768px viewport', async () => {
    const canvas = within(await DefaultStory.render());
    await userEvent.viewport({ width: 768, height: 1024 });
    expect(canvas.getByRole('navigation')).toBeInTheDocument();
  });

  it('should handle long paths gracefully at mobile viewport', async () => {
    const canvas = within(await WithLongPathsStory.render());
    await userEvent.viewport({ width: 480, height: 800 });
    expect(canvas.getByText('...')).toBeInTheDocument();
  });
});

export default {
  title: 'Molecules/Breadcrumb Navigation',
  parameters: {
    layout: 'fullscreen',
    viewport: {
      viewports: {
        mobile: { name: 'Mobile', styles: { width: '480px', height: '800px' } },
        tablet: { name: 'Tablet', styles: { width: '768px', height: '1024px' } }
      }
    }
  }
};

export const Default = {
  args: {
    items: [
      { label: 'Home', href: '/' },
      { label: 'Products', href: '/products' },
      { label: 'Current Page', href: '/products/current' }
    ]
  }
};

export const WithLongPaths = {
  args: {
    items: [
      { label: 'Home', href: '/' },
      { label: 'Very Long Category Name That Should Truncate', href: '/long-category' },
      { label: 'Current Item With Long Name', href: '/long-category/current-item' }
    ]
  }
};